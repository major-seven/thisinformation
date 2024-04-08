package data

import "github.com/major-seven/thisinformation/web/article"

type Data struct {
	Articles []*article.Article
	Dates    []string
	Today    string
	Edition  int
}
