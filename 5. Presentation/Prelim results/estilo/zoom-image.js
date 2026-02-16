document.addEventListener("DOMContentLoaded", function () {
  const overlay = document.getElementById("image-overlay");
  const overlayImg = document.getElementById("overlay-img");

  document.querySelectorAll("img.zoomable").forEach(img => {
    img.addEventListener("click", function () {
      overlayImg.src = this.src;
      overlay.style.display = "flex";
    });
  });

  overlay.addEventListener("click", function () {
    overlay.style.display = "none";
  });

  document.addEventListener("keydown", function(e) {
    if (e.key === "Escape") {
      overlay.style.display = "none";
    }
  });
});
