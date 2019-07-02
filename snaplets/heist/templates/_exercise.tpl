<bind tag="icon-accepted"><img src="/static/icons/16x16/accepted.png"/></bind>
<bind tag="icon-rejected"><img src="/static/icons/16x16/rejected.png"/></bind>
<bind tag="icon-warning"><img src="/static/icons/16x16/warning.png"/></bind>
<bind tag="icon-overdue"><img src="/static/icons/16x16/overdue.png"/></bind>
<bind tag="icon-editor"><img src="/static/icons/16x16/editor.png"/></bind>
<bind tag="valid-icon"><if-accepted><if-valid><else/><icon-warning/></if-valid></if-accepted></bind>

<apply template="_base">
  <mathjax-js/>
  <ace-editor-js/>
  <div class="description">
    <page-description/>
  </div>
  <p class="info">Linguagens: <page-languages/>.</p>  
  <!-- <apply template="_timing"/> -->
  <if-submitted>
    <!--
    <current-timing>
      <Early>
	<p><submissions-count/> submissões antecipadas; os resultados serão
	  visíveis após <valid-from/>.</p> 
      </Early>
      <default>
	-->
      <h2>Submissões anteriores</h2>
      <ol class="submissions">
	<submissions-list>
	  <li>
	    <a href="${report-url}"><submit-id/></a>&nbsp;<span class="${result-status}"><result-status/></span><valid-icon/>
	  </li>
	</submissions-list>
      </ol>
<!--  </default>
     </current-timing> -->
</if-submitted>

<h2>Nova submissão</h2>

<form id="codeform" method="POST" action="${page-url}">
  <p>
    <input type="file" id="fileselect"
	   accept="${language-extensions}"/> &nbsp;
    <input-language-selector id="langselect" name="language" form="codeform"/>
  </p>
  <p><textarea id="code" name="code" style="display:none;"/></p>
  <div id="editor"><default-text/></div>
  <p>
    <input type="submit" value="Submeter"/> &emsp;
    <a href="${page-parent-url}"
       class="button">Voltar à página de índice</a>
  </p>
</form>
</apply>


<script type="text/javascript">
  var editor = startAceEditor('editor');

  document.getElementById('codeform').addEventListener('submit',
    function() { submitListener(editor,'code'); });

  document.getElementById('fileselect').addEventListener('change',
    function() { fileListener(editor, 'fileselect', 'langselect'); });
					      
  document.getElementById('langselect').addEventListener('change',
    function() { editor.session.setMode(languageModes[this.selectedIndex]); } );
</script>

<language-constants-js/>
<default-language-js/>

<apply template="_browse">
  <li><a title="Editar a página de exercício" href="${file-url}">Editar</a></li>
</apply>
