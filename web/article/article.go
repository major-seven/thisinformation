package article

type ArticleList struct {
	Articles []*Article `json:"articles"`
}

type Article struct {
	Title   string `json:"title"`
	Content string `json:"content"`
	Author  string `json:"author"`
	Date    string `json:"date"`
}

type SortByContentLength []*Article

func (a SortByContentLength) Len() int {
	return len(a)
}

func (a SortByContentLength) Swap(i, j int) {
	a[i], a[j] = a[j], a[i]
}

func (a SortByContentLength) Less(i, j int) bool {
	return len(a[i].Content) < len(a[j].Content)
}
