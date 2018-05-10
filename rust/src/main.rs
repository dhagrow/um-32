use std::mem;
use std::fs::File;
use std::io::prelude::*;

fn main() {
    let filename = "../scrolls/sandmark.umz";
    println!("In file {}", filename);

    let mut f = File::open(filename).expect("file not found");

    let mut i = 0;
    let mut buffer = [0; 4];
    let mut platter: u32;
    loop {
        if i == 10 {
            break;
        }
        f.read_exact(&mut buffer);
        unsafe {
            platter = mem::transmute::<[u8; 4], u32>(buffer).to_be();
        }
        println!("{:?}", platter);
        i += 1;
    }
}
