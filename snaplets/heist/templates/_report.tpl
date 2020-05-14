<bind tag="icon-accepted"><img src="/static/icons/16x16/accepted.png"/></bind>
<bind tag="icon-rejected"><img src="/static/icons/16x16/rejected.png"/></bind>
<bind tag="icon-overdue"><img src="/static/icons/16x16/overdue.png"/></bind>
<bind tag="icon-editor"><img src="/static/icons/16x16/editor.png"/></bind>
<bind tag="icon-warning"><img src="/static/icons/16x16/warning.png"/></bind>
<bind tag="valid-icon"><if-valid><else/><icon-warning/></if-valid></bind>

<apply template="_browse">
  <li><a href="${page-url}" class="icon"
	 title="Voltar à página anterior">&curvearrowleft;</a></li>
  <li><button class="tablinks" onclick="openTab(event, 'report-tab')" id="report">Relatório</button></li>
  <li><button class="tablinks" onclick="openTab(event, 'editor-tab')">Editor</button></li>
</apply>


<apply template="_base">
<mathjax-js/>
<ace-editor-js/>

<div id="report-tab" class="tabcontents">
  <h1><page-title/></h1>
  <h2>Submissão <submit-id/></h2>
  <p>Enviada em <submit-time/> por <code><submit-user-id/></code>.</p>
  <if-evaluating>
    <p><img src="/static/images/spinner.svg"></p>
    <p class="info">Se a página não atualizar automaticamente,
      use o botão "reload" do "browser".</p>
    <else/>
    <if-submit-early>
      <h3>Resultado: <em>???</em></h3>
      <p>Submissão antecipada; resultado será visível
	após <submit-after/>.</p>
      <else/>
      <h3>Resultado: <em><result-status/></em>
	<if-valid><else/>&nbsp;(<em><result-check/></em>)</if-valid>
      </h3>
      <apply template="_timing"/>
      <if-feedback>
	<if-pythontutor>
	 <a href="${pythontutor-url}" 
           target="_blank"> Clicar para simular caso falhado </a>
	</if-pythontutor>
        <pre><result-report/></pre>
      </if-feedback>
    </if-submit-early>
  </if-evaluating>
</div>



<div id="editor-tab" class="tabcontents">
  <page-title/>
  <apply template="_timing"/>
  <if-allowed>
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
  </if-allowed>
</div>

<script>
function openTab(evt, tabName) {
  // Declare all variables
  var i, tabcontent, tablinks;

  // Get all elements with class="tabcontent" and hide them
  tabcontent = document.getElementsByClassName("tabcontents");
  for (i = 0; i < tabcontent.length; i++) {
    tabcontent[i].style.display = "none";
  }

  // Get all elements with class="tablinks" and remove the class "active"
  tablinks = document.getElementsByClassName("tablinks");
  for (i = 0; i < tablinks.length; i++) {
    tablinks[i].className = tablinks[i].className.replace(" active", "");
  }

  // Show the current tab, and add an "active" class to the button that opened the tab
  document.getElementById(tabName).style.display = "block";
  evt.currentTarget.className += " active";
 }

document.getElementById("report").click();
</script>
  
</apply>

