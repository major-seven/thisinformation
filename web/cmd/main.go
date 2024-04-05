package main

import (
	"database/sql"
	"html/template"
	"io"
	"net/http"
	"os"

	"github.com/labstack/echo/v4"
	"github.com/labstack/echo/v4/middleware"
	"github.com/major-seven/thisinformation/web/article"
  _ "github.com/mattn/go-sqlite3"
)

type Template struct {
	tmpl *template.Template
}

func newTemplate() *Template {
	return &Template{
		tmpl: template.Must(template.New("t").Funcs(template.FuncMap{}).ParseGlob("views/*.html")),
	}
}

func (t *Template) Render(w io.Writer, name string, data interface{}, c echo.Context) error {
	return t.tmpl.ExecuteTemplate(w, name, data)
}

type Data struct {
	Articles []*article.Article
}

func main() {
	data := Data{}

	KEY := os.Getenv("NEWS_KEY")
	if KEY == "" {
		panic("NEWS_KEY is not set")
	}

  db, err := sql.Open("sqlite3", "./news.db")
  if err != nil {
    panic(err)
  }
  defer db.Close()

  _, err = db.Exec("CREATE TABLE IF NOT EXISTS articles (id INTEGER PRIMARY KEY AUTOINCREMENT, title TEXT, content TEXT)")
  if err != nil {
    panic(err)
  }

  rows, err := db.Query("SELECT title, content FROM articles")
  if err != nil {
    panic(err)
  }
  defer rows.Close()

  for rows.Next() {
    a := new(article.Article)
    err = rows.Scan(&a.Title, &a.Content)
    if err != nil {
      panic(err)
    }
    data.Articles = append(data.Articles, a)
  }

	app := echo.New()
	app.Renderer = newTemplate()
	app.Use(middleware.Logger())
	app.Static("/static", "static")

	app.GET("/", func(c echo.Context) error {
		return c.Render(http.StatusOK, "index.html", data)
	})

	app.POST("/api/new-article/:key", func(c echo.Context) error {
		if c.Param("key") == KEY {
			a := new(article.Article)
			if err := c.Bind(a); err != nil {
				return c.String(http.StatusBadRequest, "bad request")
			}

      if a.Title == "" || a.Content == "" {
        return c.String(http.StatusBadRequest, "bad request")
      }

      _, err := db.Exec("INSERT INTO articles (title, content) VALUES (?, ?)", a.Title, a.Content)
      if err != nil {
        return c.String(http.StatusInternalServerError, "internal server error")
      }

			data.Articles = append(data.Articles, a)
			return c.String(http.StatusOK, "ok")

		} else {
			return c.String(http.StatusUnauthorized, "unauthorized")
		}
	})

  app.POST("/api/new-articles/:key", func(c echo.Context) error {
    if c.Param("key") == KEY {
      articles := new(article.ArticleList)
      if err := c.Bind(&articles); err != nil {
        return c.String(http.StatusBadRequest, "bad request")
      }

      if len(articles.Articles) == 0 {
        return c.String(http.StatusBadRequest, "bad request")
      }

      for _, a := range articles.Articles {
        if a.Title == "" || a.Content == "" {
          return c.String(http.StatusBadRequest, "bad request")
        }
      }

      for _, a := range articles.Articles {
        _, err := db.Exec("INSERT INTO articles (title, content) VALUES (?, ?)", a.Title, a.Content)
        if err != nil {
          return c.String(http.StatusInternalServerError, "internal server error")
        }
      }

      data.Articles = append(data.Articles, articles.Articles...)
      return c.String(http.StatusOK, "ok")

    } else {
      return c.String(http.StatusUnauthorized, "unauthorized")
    }
  })

	app.DELETE("/api/delete-articles/:key", func(c echo.Context) error {
		if c.Param("key") == KEY {
			data.Articles = nil
      _, err := db.Exec("DELETE FROM articles")
      if err != nil {
        return c.String(http.StatusInternalServerError, "internal server error")
      }

			return c.String(http.StatusOK, "ok")
		} else {
			return c.String(http.StatusUnauthorized, "unauthorized")
		}
	})

	app.Logger.Fatal(app.Start(":3000"))
}
