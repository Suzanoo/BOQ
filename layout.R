header <- tagList(
  div(Text(variant = "medium", "BOQ Analysis"), class = "title"),
  )

navigation <- Nav(
  groups = list(
    list(links = list(
      list(name = 'Home', url = '#!/', key = 'home', icon = 'Home'),
      list(name = 'Table', url = '#!/table', key = 'table', icon = 'Table'),
      list(name = 'WBS', url = '#!/wbs', key = 'wbs', icon = 'AnalyticsReport'),
      list(name = 'WBS by Floor Select', url = '#!/level', key = 'level', icon = 'AnalyticsReport'),
      list(name = 'Material Query', url = '#!/material', key = 'level', icon = 'AnalyticsReport'),
      list(name = 'Progress Report', url = '#!/progress', key = 'progress', icon = 'AnalyticsReport')
      ))
  ),
  initialSelectedKey = 'home',
  styles = list(
    root = list(
      height = '100%',
      boxSizing = 'border-box',
      overflowY = 'auto'
    )
  )
)

footer <- Stack(
  horizontal = TRUE,
  horizontalAlign = 'space-between',
  tokens = list(childrenGap = 20),
  Text(variant = "medium", "Built with ❤ by Appsilon", block=TRUE),
  Text(variant = "medium", nowrap = FALSE, "If you'd like to learn more, reach out to us at hello@appsilon.com"),
  Text(variant = "medium", nowrap = FALSE, "All rights reserved.")
)
  

layout <- function(mainUI){
  div(class = "grid-container",
      div(class = "header", header),
      div(class = "sidenav", navigation),
      div(class = "main", mainUI),
      div(class = "footer", footer)
  )
}