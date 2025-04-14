document.addEventListener("DOMContentLoaded", function () {
    const select = document.getElementById("language-select");
  
    if (select) {
      select.addEventListener("change", function () {
        const lang = select.value;
        const baseUrl = window.location.href.split('?')[0];
  
        if (!lang) {
          // Voltar à versão original, sem tradução
          window.location.href = baseUrl;
        } else {
          // Redirecionar com o idioma selecionado
          window.location.href = baseUrl + "?lang=" + lang;
        }
      });
  
      // Pré-selecionar idioma com base no parâmetro do URL
      const urlParams = new URLSearchParams(window.location.search);
      const currentLang = urlParams.get("lang");
      if (currentLang) {
        select.value = currentLang;
      } else {
        select.value = "";
      }
    }
  });
  
