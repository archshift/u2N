extern crate u2N;

use std::io;
use u2N::u2048;

fn calculate(line: &str) -> u2048 {
    let is_hex = |c: char| match c {
        '0'...'9' | 'x' | 'A'...'F' | 'a'...'f' => true,
        _ => false
    };
    let not_hex = |c: char| !is_hex(c);

    let op_start = line.find(not_hex).expect("No operator specified!");
    let op_end = op_start + line[op_start..].find(is_hex).expect("No second operand specified!");

    let a = line[..op_start].trim_start_matches("0x").trim();
    let b = line[op_end..].trim_start_matches("0x").trim();
    let op = line[op_start..op_end].trim();
    
    let a = u2048::from_hex(a).expect("Invalid first operand!");
    let b_2048 = u2048::from_hex(b).expect("Invalid second operand!");

    match op {
        "+" => return a + b_2048,
        "-" => return a - b_2048, 
        "*" => return a * b_2048, 
        "%" => return a % b_2048,
        _ => {}
    }

    let b_usize = usize::from_str_radix(b, 16).expect("Invalid second operand!");

    match op {
        "<<" => a << b_usize,
        ">>" => a >> b_usize,
        _ => panic!("Invalid operator!"),
    }
}

fn main() {
    let mut buf = String::new();
    let prompt = |buf: &mut String| { 
        use std::io::Write;
        print!("> ");
        io::stdout().flush().unwrap();
        buf.clear();
        io::stdin().read_line(buf)
    };
    while let Ok(s) = prompt(&mut buf) {
        if s == 0 { break }
        let out = calculate(buf.trim());
        println!("{:X}", out);
    }
}
