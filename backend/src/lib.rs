

mod java;

pub use java::JavaCodegenerator;


pub trait Codegenerator<O> {
    fn generate(&mut self, file: sb_ast::core_annotated::File) -> O;
}
