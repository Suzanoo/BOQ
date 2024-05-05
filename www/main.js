/*
https://www.youtube.com/watch?v=0EsWoEmkHog
https://dev.to/ljcdev/easy-hamburger-menu-with-js-2do0

const hamburgerBtn = document.getElementById('hamburgerBtn');
const navBar = document.getElementById('sidenav');

hamburgerBtn.addEventListener('click', function(event){
  console.log('Button clicked');
  navBar.classList.toggle('open');
});

*/

const sidenav = document.getElementById("sidenav");
const menuItems = document.querySelectorAll(".menuItem");
const hamburger= document.getElementById("hamburgerBtn");
const closeIcon= document.querySelector(".closeIcon");
const menuIcon = document.querySelector(".menuIcon");

function toggleMenu() {
  if (sidenav.classList.contains("showMenu")) {
    sidenav.classList.remove("showMenu");
    closeIcon.style.display = "none";
    menuIcon.style.display = "block";
    
  } else {
    sidenav.classList.add("showMenu");
    closeIcon.style.display = "block";
    menuIcon.style.display = "none";
 
  }
}

menuItems.forEach( 
  function(menuItem) { 
    menuItem.addEventListener("click", toggleMenu);
  }
)

hamburger.addEventListener("click", toggleMenu);

