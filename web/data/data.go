package data

import "github.com/major-seven/thisinformation/web/article"

type Data struct {
	CoverStory *article.Article
	Crossword  *article.Article
	Articles   []*article.Article
	Dates      []string
	Today      string
	Edition    int
}
