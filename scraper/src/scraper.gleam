import gleam/string
import gleam/result
import gleam/list
import shellout
import simplifile
import gleam/io
import jasper.{type JsonObject, Object, Boolean, Key, Root}
import gleam/dict

fn parse_item(item: String) -> Result(String, Nil) {
  item
  |> string.trim
  |> string.split("</title>")
  |> list.first
  |> result.map(fn(x) {
    x
    |> string.split("<title>")
    |> list.last
  })
  |> result.flatten
}

fn parse(xml: String) -> List(String) {
  xml
  |> string.split("<item>")
  |> list.drop(1)
  |> list.map(parse_item)
  |> result.values
}

fn get_titles(url: String) -> List(String) {
  shellout.command(run: "curl", with: [url], in: ".", opt: [])
  |> result.unwrap("")
  |> parse
}

fn get_date() -> String {
  let date = shellout.command(run: "date", with: [], in: ".", opt: [])
  |> result.unwrap("")
  |> string.split(" ")
  |> list.filter(fn(x) { !string.is_empty(x) })

  let year = list.last(date)
  |> result.unwrap("")
  let date = list.take(date, 3)

  list.append(date, [year])
  |> string.join(" ")
  |> string.drop_right(1)
}

fn get_article(title: String, date: String) -> Result(String, Nil) {
  let json = [#("model", jasper.String("reporter")), #("prompt", jasper.String("date: " <> date <> " title: " <> title)), #("stream", Boolean(False))]
  |> dict.from_list

  let json_string = Object(json)
  |> jasper.stringify_json

  io.println("Getting: " <> json_string)

  let res = shellout.command(run: "curl", with: ["http://localhost:11434/api/generate", "-d", json_string, "-s"], in: ".", opt: [])
  |> result.unwrap("NO ARTICLE")
  |> jasper.parse_json
  |> result.map(fn(x) {
    jasper.query_json(x, Key(Root, "response"))
  })

  case res {
    Ok(Ok(jasper.String(x))) -> Ok(x)
    _ -> Error(Nil)
  }
}

fn get_best_titles(titles: String) -> Result(String, Nil) {
  let json = [#("model", jasper.String("llama2")), #("prompt", jasper.String("You are the editor of a newspaper. Select the 10 best titles from this list: Do not add anything to the titles, just select them. Just give me a numbered list back without commentary." <> titles)), #("stream", Boolean(False))]
  |> dict.from_list

  let json_string = Object(json)
  |> jasper.stringify_json

  io.println("Getting best titles")

  let res = shellout.command(run: "curl", with: ["http://localhost:11434/api/generate", "-d", json_string, "-s"], in: ".", opt: [])
  |> result.unwrap("NO TITLE")
  |> jasper.parse_json
  |> result.map(fn(x) {
    jasper.query_json(x, Key(Root, "response"))
  })

  case res {
    Ok(Ok(jasper.String(x))) -> Ok(x)
    _ -> Error(Nil)
  }
}

fn get_new_title(text: String) -> Result(String, Nil) {
  let cropped = string.to_graphemes(text)
  |> list.filter(fn(x) { x != "\n" && x != "\""})
  |> string.join("")

  let json = [#("model", jasper.String("llama2")), #("prompt", jasper.String("write one title for the following text, do not give me options: " <> cropped)), #("stream", Boolean(False))]
  |> dict.from_list

  let json_string = Object(json)
  |> jasper.stringify_json

  io.println("Getting title")

  let res = shellout.command(run: "curl", with: ["http://localhost:11434/api/generate", "-d", json_string, "-s"], in: ".", opt: [])
  |> result.unwrap("NO TITLE")
  |> jasper.parse_json
  |> result.map(fn(x) {
    jasper.query_json(x, Key(Root, "response"))
  })

  case res {
    Ok(Ok(jasper.String(x))) -> Ok(x)
    _ -> Error(Nil)
  }
}

pub fn main() {
  let json = [#("title", jasper.String("hello")), #("text", jasper.String("world"))]
  |> dict.from_list
  let json_string = Object(json)
  |> jasper.stringify_json

  let res = shellout.command(run: "curl", with: ["http://localhost:3000/api/new-article/123", "-d", json_string, "-s", "-X", "POST"], in: ".", opt: [])
  |> io.debug
  panic as "end"

  let rss = "sources.txt"
  |> simplifile.read
  |> result.unwrap("")
  |> string.split("\n")

  let titles = rss
  |> list.map(get_titles)
  |> list.flatten

  let best_choice = titles
  |> string.join(", ")
  |> get_best_titles
  |> result.unwrap("")
  |> string.split("\n")
  |> list.drop(2)
  |> list.map(fn(x) {
      x
      |> string.split(". ")
      |> list.last
      |> result.unwrap("NOTHING")
      |> string.to_graphemes
      |> list.filter(fn(x) { x != "\"" })
      |> string.join("")
    })

  let best_choice = list.split(best_choice, 10).0

  let date = get_date()

  best_choice
  |> list.map(fn(x) {
      get_article(x, date)
      |> result.map(fn(text) {
        let title = get_new_title(text)
        |> result.unwrap("error")
        |> string.to_graphemes
        |> list.filter(fn(x) { x != "\\" && x != "\"" && x != "\n"})
        |> string.join("")

        let json = [#("title", jasper.String(title)), #("text", jasper.String(text))]
        |> dict.from_list
        let json_string = Object(json)
        |> jasper.stringify_json

        let res = shellout.command(run: "curl", with: ["http://localhost:3000/api/new-article/123", "-d", json_string, "-s", "-X", "POST"], in: ".", opt: [])
        |> io.debug

        simplifile.write("../articles/" <> title <> ".txt", text)
      })
    })
}
