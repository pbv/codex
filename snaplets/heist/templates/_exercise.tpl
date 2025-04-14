<bind tag="icon-warning">&#9888;</bind>
<bind tag="valid-icon"><if-valid><else/><icon-warning/></if-valid></bind>
<!-- -->
<apply template="_base">
  <mathjax-js/>
  <ace-editor-js/>

  <apply template="_browse">
    <if-parent>
      <li><a href="${page-parent-url}" class="icon"
	   title="Voltar à página anterior">&curvearrowleft;</a></li>
    </if-parent>
    <li><button class="tablinks" onclick="openTab(event, 'description-tab')" id="description">Descrição</button></li>
    <li><button class="tablinks" onclick="openTab(event, 'editor-tab')">Editor</button></li>
    <li><button class="tablinks" onclick="openTab(event, 'submissions-tab')">Submissões</button><li>
    <li>
      <div id="translation-select">
        <label for="language-select"> Traduzir:</label>
        <select id="language-select" onchange="changeLanguage()">
          <option value="">Original</option>
          <option value="pt">Português</option>
          <option value="en">English</option>
          <option value="fr">Français</option>
          <option value="es">Español</option>
        </select>
      </div>
    </li>
  </apply>

  <div id="description-tab" class="tabcontents">
    <page-description/>
    <p>Submissões: <em><timing/></em></p>
    <if-max-attempts>
      <p>Tentativas disponíveis: <em><submissions-remain/></em></p>
    </if-max-attempts>
  </div>

  <div id="submissions-tab" class="tabcontents">
    <page-title/>
    <h3>Submissões</h3>
    <p><submissions-count/> submissões efetuadas.</p>
    <ol class="submissions">
      <submissions-list>
	<li> <a href="${report-url}"><valid-icon/><submit-id/>&nbsp;<span class="${result-status}"><result-status/></span></a>
	</li>
      </submissions-list>
    </ol>
  </div>

  <div id="editor-tab" class="tabcontents">
    <page-title/>
    <if-available>
      <form id="codeform" method="POST" action="${page-url}">
	<p>
	  <input type="file" id="fileselect"
		 accept="${language-extensions}"/> &nbsp;
	  <input-language-selector
	    id="langselect"  name="language"
	    form="codeform"  /> &emsp;
	  <input type="submit" value="Re-submeter"/>
	  &emsp;&emsp;&emsp;&emsp;&emsp;
	  <apply template="_fontsize"/>
	</p>
	<p><textarea id="code" name="code" style="display:none;"/></p>
	<div id="editor"><default-text/></div>
      </form>  
      <else/>
      <p>Submissões indisponíveis.</p>
    </if-available>
  </div>

<script type="text/javascript" src="/static/js/tabview.js"/>
<script type="text/javascript" src="/static/js/ace-start.js"/>
<script type="text/javascript" src="/static/js/changelang.js"/>
<language-constants-js/>
<default-language-js/>
<script type="text/javascript">
document.getElementById("description").click();
</script>

</apply>
