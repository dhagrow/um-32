import argv
import file_streams/file_stream
import file_streams/file_stream_error

import gleam/dict
import gleam/int
import gleam/io
import gleam/list
import gleam/result

pub type Operator {
  Standard(code: Int, a: Int, b: Int, c: Int)
  Ort(code: Int, a: Int, value: Int)
}

pub type State {
  State(
    memory: dict.Dict(Int, dict.Dict(Int, Int)),
    reg: dict.Dict(Int, Int),
    finger: Int,
    op_count: Int,
  )
}

pub fn main() {
  let state =
    State(
      memory: dict.new(),
      reg: list.range(0, 7)
        |> list.map(fn(k) { #(k, 0) })
        |> dict.from_list,
      finger: 0,
      op_count: 0,
    )

  case argv.load().arguments {
    [program] -> {
      let assert Ok(stream) = file_stream.open_read(program)
      let assert Ok(platter) = load(stream, 0, dict.new())
      let state =
        State(..state, memory: state.memory |> dict.insert(0, platter))
      cycle(state)
    }
    _ -> {
      io.println("usage: ./machine <program>")
      Error(Nil)
    }
  }
}

fn cycle(state: State) {
  echo dump_state(state)

  use array <- result.try(state.memory |> dict.get(0))
  use platter <- result.try(array |> dict.get(state.finger))

  let code = int.bitwise_shift_right(platter, 28)

  let state = case code {
    13 -> {
      let a = int.bitwise_shift_right(platter, 25) |> int.bitwise_and(7)
      let value = int.bitwise_and(platter, 0x1ffffff)
      echo #(code, a, value)
      State(..state, reg: state.reg |> dict.insert(a, value))
    }
    _ -> {
      let a = int.bitwise_shift_right(platter, 6) |> int.bitwise_and(7)
      let b = int.bitwise_shift_right(platter, 3) |> int.bitwise_and(7)
      let c = platter |> int.bitwise_and(7)

      echo #(code, a, b, c)

      case code {
        // cmv
        0 -> {
          case state.reg |> get(c) {
            0 -> state
            _ -> {
              State(
                ..state,
                reg: state.reg |> dict.insert(a, state.reg |> get(b)),
              )
            }
          }
        }
        // aix
        1 -> {
          State(
            ..state,
            reg: state.reg
              |> dict.insert(
                a,
                state.memory
                  |> get(state.reg |> get(b))
                  |> get(state.reg |> get(c)),
              ),
          )
        }
        // aam
        2 -> {
          let reg_a = state.reg |> get(a)
          let array =
            state.memory
            |> get(reg_a)
            |> dict.insert(state.reg |> get(b), state.reg |> get(c))
          State(..state, memory: state.memory |> dict.insert(reg_a, array))
        }
        // add
        3 -> {
          State(
            ..state,
            reg: case
              int.modulo(
                { state.reg |> get(b) } + { state.reg |> get(c) },
                4_294_967_296,
              )
            {
              Ok(v) -> state.reg |> dict.insert(a, v)
              Error(_) -> todo
            },
          )
        }
        // // mul
        // 4 -> Nil
        // dvi
        5 -> {
          State(
            ..state,
            reg: case
              int.floor_divide(state.reg |> get(b), state.reg |> get(c))
            {
              Ok(v) -> state.reg |> dict.insert(a, v)
              Error(e) -> {
                echo #("error", e)
                todo
              }
            },
          )
        }
        // nad
        6 -> {
          State(
            ..state,
            reg: state.reg
              |> dict.insert(
                a,
                int.bitwise_exclusive_or(
                  int.bitwise_and(state.reg |> get(b), state.reg |> get(c)),
                  4_294_967_295,
                ),
              ),
          )
        }
        // // hlt
        // 7 -> Nil
        // // alc
        // 8 -> Nil
        // // abd
        // 9 -> Nil
        // // otp
        // 10 -> Nil
        // // inp
        // 11 -> Nil
        // lod
        12 -> {
          let index = state.reg |> get(b)
          State(..state, memory: case index {
            0 -> state.memory
            _ -> {
              state.memory |> dict.insert(0, state.memory |> get(index))
            }
          })
        }
        _ -> todo
      }
    }
  }

  let state =
    State(..state, finger: state.finger + 1, op_count: state.op_count + 1)
  cycle(state)
}

fn load(stream, pos, platter) {
  // read 4-byte chunks until EOF
  case file_stream.read_bytes(stream, 4) {
    Ok(data) -> {
      let value = case data {
        <<value:unsigned-big-size(32)>> -> value
        _ -> 0
      }
      load(stream, pos + 1, platter |> dict.insert(pos, value))
    }
    Error(file_stream_error.Eof) -> Ok(platter)
    Error(e) -> Error(e)
  }
}

fn get(d, k) {
  case d |> dict.get(k) {
    Ok(v) -> v
    Error(_) -> todo
  }
}

fn dump_state(state: State) {
  #(state.finger, state.op_count, state.reg |> dict.values())
}
