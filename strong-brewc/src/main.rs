mod typechecker;

use backend::JavaCodegenerator;
use backend::Codegenerator;
use clap::Parser;

#[derive(Parser, Debug)]
struct Args {

    file: String,
    // TODO: add platofrm specific paths
    #[arg(short, long, default_value = "strongbrew")]
    stdlib_path: String,
    #[arg(short, long, default_value = ".strongbrew")]
    cache_path: String,
    #[arg(short, long, default_value = "out")]
    output_path: String,
}

fn grab_imports<'a>(args: &Args, name: String, body: String, file: &sb_ast::outer::File<'a>) -> Vec<(String, String)> {
    let mut files = vec![];
    let imports = file.get_imports();
    for import in imports {

        let path = import.segments.join("/");
        let local_path = format!("{}/{}.sb", ".", &path);
        let stdlib_path = format!("{}/{}.sb", &args.stdlib_path, &path);

        if std::path::Path::new(&local_path).exists() {
            let body = std::fs::read_to_string(&local_path).unwrap();
            let file = parser::parse(&local_path, &body);
            if let Err(_) = file {
                todo!("Report error")
            }
            let file = file.unwrap();
            files.extend(grab_imports(args, local_path, body.clone(), &file));
            continue;
        }

        if std::path::Path::new(&stdlib_path).exists() {
            let body = std::fs::read_to_string(&stdlib_path).unwrap();
            let file = parser::parse(&stdlib_path, &body);
            if let Err(_) = file {
                todo!("Report error")
            }
            let file = file.unwrap();
            files.extend(grab_imports(args, stdlib_path, body.clone(), &file));
            continue;
        }
    }

    files.push((name, body));
    
    files
}



fn main() {

    let args = Args::parse();

    let file_data = std::fs::read_to_string(&args.file).unwrap();

    let result = parser::parse(&args.file, &file_data);
    if let Err(_) = result {
        std::process::exit(1);
    }
    let file = result.unwrap();
    let files = grab_imports(&args, args.file.clone(), file_data.clone(), &file);

    let env_path = format!("{}/os/env.sb", args.stdlib_path);
    println!("{}", env_path);
    let env_data = std::fs::read_to_string(&env_path).unwrap();
    
    let env = parser::parse(&env_path, &env_data);
    if let Err(_) = env {
        std::process::exit(1);
    }
    let env = env.unwrap();
    let mut env_grabbed_imports = grab_imports(&args, env_path.clone(), env_data.clone(), &env);
    env_grabbed_imports.extend(files);
    let files = env_grabbed_imports;
    //println!("{:#?}", files);
    //parsed_files.push((&env_path, env)); 

    let mut parsed_files = Vec::new();



    for (name, data) in files.iter() {
        println!("Parsing file: {}", name);
        let result = parser::parse(name, data);
        if let Err(_) = result {
            std::process::exit(1);
        }
        parsed_files.push((name, result.unwrap()));
    }
    let core_path = format!("{}/core/core.sb", args.stdlib_path);
    let core_data = std::fs::read_to_string(&core_path).unwrap();
    
    let core = parser::parse(&core_path, &core_data);
    if let Err(_) = core {
        std::process::exit(1);
    }
    let core = core.unwrap();
    parsed_files.insert(0, (&core_path, core));

    
    println!("Restricting Ast");

    let files = parsed_files.into_iter()
        .map(|(name, file)| {
            let result = sb_ast::convert_outer_to_inner(file);
            if let Err(_) = result {
                todo!("report error");
            }
            
            (name, result.unwrap())
        });

    println!("Desugaring Ast");
    let files = files.map(|(name, file)| {
        let result = sb_ast::convert_inner_to_core(file);
        (name, result)
    }).collect::<Vec<_>>();

    println!("Typechecking Ast");

    let mut typechecker = typechecker::TypeChecker::new();
    let names = files.iter().map(|(name, _)| name).collect::<Vec<_>>();
    let files = typechecker.check_files(&files);
    if let Err(e) = files {
        println!("{:?}", e);
        std::process::exit(1);
    }
    let files = files.unwrap();
    let mut code_generator = JavaCodegenerator::new();
    for (file, name) in files.into_iter().zip(names) {
        //println!("\t\t{}", name);
        let code_files = code_generator.generate(file);

        for (class_name, code) in code_files {

            let name = name.split("./").collect::<Vec<_>>();
            let split = if name.len() > 1 {
                name[1].split('/')
            } else {
                name[0].split('/')
            };
            let mut path = String::from(&args.cache_path);
            path.push('/');
            for item in split {
                //println!("{}", item);
                if !item.contains('.') {
                    path.push_str(item);
                    path.push('/');
                } else {
                    std::fs::create_dir_all(&path).unwrap();
                    let mut file_name = item.split('.').next().unwrap();
                    file_name = if !class_name.is_empty() {
                        class_name.as_str()
                    } else {
                        file_name
                    };
                    path.push_str(&format!("{}.java", file_name));
                }
            }
            std::fs::write(&path, code).unwrap();
        }

    }

    let start_path = format!("{}/Start.java", args.stdlib_path);
    let start_output_path = format!("{}/Start.java", args.cache_path);

    std::fs::copy(start_path, &start_output_path).unwrap();

    let mut_path = format!("{}/Mut.java", args.stdlib_path);
    let mut_output_path = format!("{}/strongbrew/Mut.java", args.cache_path);

    std::fs::copy(mut_path, &mut_output_path).unwrap();

    copy_library_files(&args, "tuples").unwrap();
    copy_library_files(&args, "callables").unwrap();
    copy_library_files(&args, "numbers").unwrap();


    let output = std::process::Command::new("javac")
        .arg("-d")
        .arg(&args.output_path)
        .arg("-classpath")
        .arg(&args.cache_path)
        .arg(start_output_path)
        .output()
        .expect("Failed to run java compiler");


    let stdout = String::from_utf8(output.stderr).unwrap();
    
    println!("{}", stdout);

}

fn copy_library_files(args: &Args, name: &str) -> std::io::Result<()> {

    let lib_path = format!("{}/{}", args.stdlib_path, name);
    let lib_output_path = format!("{}/{}", args.cache_path, lib_path);

    std::fs::create_dir_all(&lib_output_path)?;
    
    for dir in std::fs::read_dir(&lib_path)? {
        let dir = dir?;


        let path = dir.path();
        let input_path = path.as_path().to_str().unwrap();
        let output_path = format!("{}/{}", args.cache_path, dir.path().as_path().to_str().unwrap());
        std::fs::copy(input_path, output_path)?;
    }
    Ok(())
}
