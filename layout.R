header <- tagList(
  div(class='title', "BOQ ANALYSIS"),
  div(
    HTML('
         <button id="hamburgerBtn">
            <!-- material icons https://material.io/resources/icons/ -->
            <i class="menuIcon material-icons">menu</i>
            <i class="closeIcon material-icons">close</i>
        </button>
         ')
  )
  )

navigation <- Nav(class='menu',
  groups = list(
    list(
      links = list(
        list(class='menuItem',name = 'Home', url = '#!/', key = 'home', icon = 'Home'),
        list(class='menuItem',name = 'Table', url = '#!/table', key = 'table', icon = 'Table'),
        list(class='menuItem',name = 'WBS', url = '#!/wbs', key = 'wbs', icon = 'BulletedTreeList'),
        list(class='menuItem',name = 'WBS by Floor Select', url = '#!/level', key = 'level', icon = 'QueryList'),
        list(class='menuItem',name = 'Material Query', url = '#!/material', key = 'level', icon = 'SearchData'),
        list(class='menuItem',name = 'Progress Report', url = '#!/progress', key = 'progress', icon = 'Chart'
             )
      ))
  ),
  initialSelectedKey = 'home', 
  styles = list(
    root = list(
      height = '100%',
      boxSizing = 'border-box',
      border = "1px solid #eee",
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
  div(class="grid-container",
    div(class='header', header),
    div(id ='sidenav', navigation),
    div(id = "main_content", mainUI),
    div(class = "footer", footer)
  )
}