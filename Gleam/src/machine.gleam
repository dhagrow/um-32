import argv
import file_streams/file_stream
import file_streams/file_stream_error
import gleave

import gleam/bit_array
import gleam/dict
import gleam/int
import gleam/io
import gleam/list
import gleam/result
import gleam/set
import gleam/string

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
    next_index: Int,
    abandoned_indexes: set.Set(Int),
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
      next_index: 0,
      abandoned_indexes: set.new(),
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
  use array <- result.try(state.memory |> dict.get(0))
  use platter <- result.try(array |> dict.get(state.finger))

  let code = int.bitwise_shift_right(platter, 28)

  let state = case code {
    13 -> {
      let a = int.bitwise_shift_right(platter, 25) |> int.bitwise_and(7)
      let value = int.bitwise_and(platter, 0x1ffffff)

      // echo dump_state(code, #(a, value), state)

      State(..state, reg: state.reg |> dict.insert(a, value))
    }
    _ -> {
      let a = int.bitwise_shift_right(platter, 6) |> int.bitwise_and(7)
      let b = int.bitwise_shift_right(platter, 3) |> int.bitwise_and(7)
      let c = platter |> int.bitwise_and(7)

      // echo dump_state(code, #(a, b, c), state)

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
              Error(_) -> panic as "add error"
            },
          )
        }
        // mul
        4 -> {
          State(
            ..state,
            reg: case
              int.modulo(
                { state.reg |> get(b) } * { state.reg |> get(c) },
                4_294_967_296,
              )
            {
              Ok(v) -> {
                state.reg |> dict.insert(a, v)
              }
              Error(_) -> panic as "mul error"
            },
          )
        }
        // dvi
        5 -> {
          State(
            ..state,
            reg: case
              int.floor_divide(state.reg |> get(b), state.reg |> get(c))
            {
              Ok(v) -> state.reg |> dict.insert(a, v)
              Error(_) -> panic as "dvi error"
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
        // hlt
        7 -> {
          gleave.exit(0)
          state
        }
        // alc
        8 -> {
          let #(index, abandoned_indexes) = case
            set.to_list(state.abandoned_indexes)
          {
            [] -> #(state.next_index + 1, [])
            [head, ..tail] -> #(head, tail)
          }
          State(
            ..state,
            memory: state.memory
              |> dict.insert(
                index,
                list.range(0, state.reg |> get(c))
                  |> list.map(fn(k) { #(k, 0) })
                  |> dict.from_list,
              ),
            reg: state.reg |> dict.insert(b, index),
            next_index: int.max(state.next_index, index),
            abandoned_indexes: set.from_list(abandoned_indexes),
          )
        }
        // abd
        9 -> {
          let index = state.reg |> get(c)
          State(
            ..state,
            memory: state.memory |> dict.delete(index),
            abandoned_indexes: state.abandoned_indexes |> set.insert(index),
          )
        }
        // otp
        10 -> {
          let reg_c = state.reg |> get(c)
          io.print(case bit_array.to_string(<<reg_c>>) {
            Ok(v) -> v
            Error(_) -> ""
          })
          state
        }
        // inp
        11 -> todo
        // lod
        12 -> {
          let index = state.reg |> get(b)
          State(
            ..state,
            memory: case index {
              0 -> state.memory
              _ -> {
                state.memory |> dict.insert(0, state.memory |> get(index))
              }
            },
            finger: { state.reg |> get(c) } - 1,
          )
        }
        _ -> panic as "unexpected opcode"
      }
    }
  }

  cycle(State(..state, finger: state.finger + 1, op_count: state.op_count + 1))
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
    Error(_) ->
      panic as string.concat(["dict key does not exist: ", int.to_string(k)])
  }
}
// fn dump_state(code, args, state: State) {
//   #(code, args, state.finger, state.op_count, state.reg |> dict.values())
// }
