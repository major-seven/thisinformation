import gleam/string
import gleam/result
import gleam/list
import shellout
import simplifile

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
  |> list.filter(fn(x) { !string.contains(x, "scienceNOW") && !string.contains(x, "Watch the Program") })
}

fn get_titles(url: String) -> List(String) {
  shellout.command(run: "curl", with: [url], in: ".", opt: [])
  |> result.unwrap("")
  |> parse
}

pub fn main() {
  let rss = "urls.txt"
  |> simplifile.read
  |> result.unwrap("")
  |> string.split("\n")

  rss
  |> list.map(get_titles)
  |> list.flatten
  |> string.join("\n")
  |> simplifile.write(to: "../titles.txt")
}
