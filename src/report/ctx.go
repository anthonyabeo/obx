package report

type Context struct {
	FileName string
	Source   *SourceManager
	Reporter Reporter
	TabWidth int
}
