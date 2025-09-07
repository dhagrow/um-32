use std::io;
use std::env;
use std::mem;
use std::char;
use std::fs::File;
use std::vec::Vec;
use std::io::prelude::*;

#[allow(dead_code)]
#[allow(non_camel_case_types)]
#[derive(Debug)]
enum Operator {
    cmv, aix, aam, add, mul, dvi, nad, hlt, alc, abd, otp, inp, lod, ort,
}

fn operator_from_u8(n: u8) -> Option<Operator> {
    if n <= 13 {
        Some(unsafe { mem::transmute(n) })
    } else {
        None
    }
}

struct Machine {
    memory: Vec<Vec<u32>>,
    reg: [u32; 8],
    abandoned_indexes: Vec<u32>,
}

impl Machine {
    fn new() -> Machine {
        Machine{
            memory: vec![],
            reg: [0; 8],
            abandoned_indexes: vec![],
            }
    }

    fn load(&mut self, filename: &String) {
        let mut f = File::open(filename).expect("file not found");

        let mut buffer = [0; 4];
        let mut platter: u32;

        let mut program = vec![];

        while f.read_exact(&mut buffer).is_ok() {
            platter = u32::from_be_bytes(buffer);
            program.push(platter);
        }

        self.memory.push(program);
    }

    fn run(&mut self) {
        let reg = &mut self.reg;
        let mut finger: u32 = 0;
        let mut platter: u32;

        let mut code: u8;
        let mut a: usize;
        let mut b: usize;
        let mut c: usize;
        let mut val: u32;

        loop {
            platter = self.memory[0][finger as usize];

            code = (platter >> 28) as u8;

            match operator_from_u8(code) {
                Some(Operator::ort) => {
                    a = ((platter >> 25) & 7) as usize;
                    val = platter & 0x1ffffff;

                    reg[a] = val as u32;
                },
                Some(code) => {
                    a = ((platter >> 6) & 7) as usize;
                    b = ((platter >> 3) & 7) as usize;
                    c = (platter & 7) as usize;

                    // Machine::state(&self.memory, reg, &code, a, b, c);

                    match code {
                        Operator::cmv => {
                            if reg[c] != 0 {
                                reg[a] = reg[b];
                            }
                        },
                        Operator::aix => reg[a] = self.memory[reg[b] as usize][reg[c] as usize],
                        Operator::aam => self.memory[reg[a] as usize][reg[b] as usize] = reg[c],
                        Operator::add => reg[a] = reg[b].wrapping_add(reg[c]),
                        Operator::mul => reg[a] = reg[b].wrapping_mul(reg[c]),
                        Operator::dvi => reg[a] = reg[b].wrapping_div(reg[c]),
                        Operator::nad => reg[a] = !(reg[b] & reg[c]),
                        Operator::hlt => break,
                        Operator::alc => {
                            let new_mem = vec![0; reg[c] as usize];
                            match self.abandoned_indexes.pop() {
                                Some(index) => {
                                    self.memory[index as usize] = new_mem;
                                    reg[b] = index;
                                },
                                None => {
                                    self.memory.push(new_mem);
                                    reg[b] = (self.memory.len()-1) as u32;
                                },
                            };
                        },
                        Operator::abd => {
                            self.memory[reg[c] as usize].clear();
                            self.abandoned_indexes.push(reg[c]);
                        },
                        Operator::otp => {
                            print!("{}", char::from_u32(reg[c]).unwrap());
                            io::stdout().flush().unwrap();
                        },
                        Operator::inp => {
                            reg[c] = io::stdin().bytes().next().unwrap().unwrap() as u32;
                        },
                        Operator::lod => {
                            if reg[b] != 0 {
                                self.memory[0] = self.memory[reg[b] as usize].to_vec();
                            }
                            finger = reg[c].wrapping_sub(1);
                        },
                        _ => {
                            println!("unknown code: {:?}", code);
                            break;
                        }
                    }
                },
                None => {
                    println!("unknown code: {}", code);
                    break;
                },
            }

            finger = finger.wrapping_add(1);
        }
    }

    #[allow(dead_code)]
    fn state(mem: &Vec<Vec<u32>>, reg: &[u32; 8], code: &Operator, a: usize, b: usize, c: usize) {
        println!("{:?}({}, {}, {})", code, a, b, c);
        println!("reg {:?}", reg);
        println!("mem {:?}", mem.len());
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        println!("usage: machine <source>");
        return;
    }

    let filename = &args[1];

    let mut machine = Machine::new();
    machine.load(&filename);
    machine.run();
}
