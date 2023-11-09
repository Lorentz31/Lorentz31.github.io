window.addEventListener('scroll', function() {
    const navbar = document.querySelector('.navbar');
    const scrollPos = window.scrollY;

    if (scrollPos > 100) {
        navbar.classList.add('scrolled'); // Add a class for transparency
    } else {
        navbar.classList.remove('scrolled'); // Remove the class to revert to the original state
    }
});