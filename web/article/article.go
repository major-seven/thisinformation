package article

type ArticleList struct {
  Articles []*Article `json:"articles"`
}

type Article struct {
	Content string `json:"content"`
	Title   string `json:"title"`
}
