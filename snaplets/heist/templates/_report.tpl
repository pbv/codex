<bind tag="icon-accepted"><img src="/static/icons/16x16/accepted.png"/></bind>
<bind tag="icon-rejected"><img src="/static/icons/16x16/rejected.png"/></bind>
<bind tag="icon-overdue"><img src="/static/icons/16x16/overdue.png"/></bind>
<bind tag="icon-editor"><img src="/static/icons/16x16/editor.png"/></bind>
<bind tag="valid-icon"><if-accepted><if-valid><else/><icon-warning/></if-valid></if-accepted></bind>

<apply template="_base">
  <mathjax-js/>
  <ace-editor-js/>
<h1><page-title/></h1>
<h2>Submissão <submit-id/></h2>
<p>Enviada por <code><submit-user-id/></code> em <submit-time/>.</p>
<if-evaluating>
  <p><img src="/static/images/spinner.svg"></p>
  <p class="info">Se a página não atualizar automaticamente,
      use o botão "reload" do "browser".</p>
  <p><a class="button"
     href="${page-url}">Voltar ao exercício</a> &emsp;
  <a class="button"
       href="${page-parent-url}">Voltar à página de índice</a>
  </p>
  <else/>
  <!-- 
<current-timing>
    <Early>
      <p>Submissão antecipada; o resultado ficará visível após <valid-from/>.</p>
    </Early>
    <default>
-->
      <h3>Resultado: <em><result-status/></em><if-valid><else/><result-check/></if-valid></h3>
      <if-feedback><pre><result-report/></pre></if-feedback>
      <!-- 
    </default>
</current-timing>
-->

<h2>Editar submissão</h2>

<form id="codeform" method="POST" action="${page-url}">
  <p>
    <input type="file" id="fileselect"
	   accept="${language-extensions}"/> &nbsp;
    <input-language-selector
      id="langselect"  name="language"
      form="codeform"  selected="${submit-lang}"/>
  </p>
  <p><textarea id="code" name="code" style="display:none;"/></p>
  <div id="editor"><submit-text/></div>
  <p>
    <input type="submit" value="Re-submeter"/> &emsp;
    <a class="button"
       href="${page-url}">Voltar ao exercício</a> &emsp;
    <a class="button"
       href="${page-parent-url}">Voltar à página de índice</a>
  </p>
</form>

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

</if-evaluating>
</apply>

<apply template="_browse">
  <li><a title="Editar folha de exercício"
     href="${file-url}">Editar</a><li>
</apply>
