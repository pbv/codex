<span id="translation-select">
  <label for="language-select">Language:</label>
  <select id="language-select" onchange="changeLanguage()">
    <option value=""><deepL-default-language/></option>
    <deepL-languages>
      <option value="${language}"><language/></option>
    </deepL-languages>
  </select>
</span>
