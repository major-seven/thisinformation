package article

type ArticleList struct {
  Articles []*Article `json:"articles"`
}

type Article struct {
	Title   string `json:"title"`
	Content string `json:"content"`
}
