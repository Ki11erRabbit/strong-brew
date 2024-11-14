





letters = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'


def generate_tuple_def(letters):
    lst = []
    lst.append(f"package strongbrew.tuples;")
    lst.append(f"public class Tuple{len(letters)} <{', '.join([f'{c}' for c in letters])}> {{")
    for i, c in enumerate(letters):
        lst.append(f"    public final {c.upper()} {c.lower()};")
    
    lst.append(f"    public Tuple{len(letters)}({', '.join([f'{c} {c.lower()}' for c in letters])}) {{")
    for c in letters:
        lst.append(f"        this.{c.lower()} = {c.lower()};")
    lst.append("    }")
    lst.append("}")
    return f"Tuple{len(letters)}", lst

def generate_callable_def(letters):
    shortened = letters[:len(letters) -1]
    lst = []
    lst.append(f"package strongbrew.callables;")
    lst.append(f"@FunctionalInterface")
    lst.append(f"public interface Callable{len(letters) - 1} <{', '.join([f'{c}' for c in letters])}> {{")
    lst.append(f"    public {letters[-1]} call({', '.join([f'{c} {c.lower()}' for c in shortened])});")
    lst.append(f"}}")
    return f"Callable{len(letters) - 1}", lst

def generate_callable0_def():
    lst = []
    lst.append(f"package strongbrew.callables;")
    lst.append(f"@FunctionalInterface")
    lst.append(f"public interface Callable0 <A> {{")
    lst.append(f"    public A call();")
    lst.append(f"}}")
    return f"Callable0", lst
size = 2

name, body = generate_callable0_def()
with open("strongbrew/callables/" + name + ".java", "w") as f:
    f.write('\n'.join(body))

while size <= len(letters):
    name, body = generate_tuple_def(letters[:size])
    with open("strongbrew/tuples/" + name + ".java", "w") as f:
        f.write('\n'.join(body))
    name, body = generate_callable_def(letters[:size])
    with open("strongbrew/callables/" + name + ".java", "w") as f:
        f.write('\n'.join(body))
    size += 1
    
