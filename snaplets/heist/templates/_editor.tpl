<page-title/>
<if-available>
  <form id="codeform" method="POST" action="${page-url}">
    <p>
      <input type="file" id="fileselect"
	     accept="${language-extensions}"/> &nbsp;
      <input-language-selector
	id="langselect"  name="language"
	form="codeform"  selected="${submit-lang}"/> &emsp;
      <input type="submit" value="Re-submeter"/>
      &emsp;&emsp;&emsp;&emsp;&emsp;
      <apply template="_fontsize"/>
    </p>
    <p><textarea id="code" name="code" style="display:none;"/></p>
    <div id="editor"><submit-code/></div>
  </form>  
  <else/>
    <p>Submissões indisponíveis.</p>
</if-available>
