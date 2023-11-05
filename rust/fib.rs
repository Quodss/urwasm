//mod utils;
// use wat::parse_str;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn fib(n: u32) -> u32 {
     if n == 0 {
          return 0;
      } else if n == 1 {
          return 1;
      } else {
          let mut a = 0;
          let mut b = 1;
          for _ in 2..=n {
              let temp = a + b;
              a = b;
              b = temp;
          }
          return b;
      }
  }