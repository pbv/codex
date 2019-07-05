<bind tag="icon-accepted"><img src="/static/icons/16x16/accepted.png"/></bind>
<bind tag="icon-rejected"><img src="/static/icons/16x16/rejected.png"/></bind>
<bind tag="icon-warning"><img src="/static/icons/16x16/warning.png"/></bind>
<bind tag="icon-overdue"><img src="/static/icons/16x16/overdue.png"/></bind>
<bind tag="icon-editor"><img src="/static/icons/16x16/editor.png"/></bind>
<bind tag="valid-icon"><if-valid><else/><icon-warning/></if-valid></bind>

<apply template="_browse">
  <if-parent>
    <li><a href="${page-parent-url}"
	   title="Voltar à página anterior">&curvearrowleft;</a></li>
  </if-parent>
  <li><button class="tablinks" onclick="openTab(event, 'description-tab')" id="description">Descrição</button></li>
  <li><button class="tablinks" onclick="openTab(event, 'editor-tab')">Editor</button></li>
  <li><button class="tablinks" onclick="openTab(event, 'submissions-tab')">Submissões</button><li>
    <!--
  <ifAdmin>
    <li><a class="admin"
	   title="Editar esta página" href="${file-url}">Editar</a></li>
  </ifAdmin>
  -->
</apply>

<apply template="_base">
  <mathjax-js/>
  <ace-editor-js/>
 
  <div id="description-tab" class="tabcontents">
    <page-description/>
  </div>

  <div id="submissions-tab" class="tabcontents">
    <page-title/>
    <if-submitted>
	<h3>Submissões anteriores</h3>
	<if-early>
	  <p><submissions-count/> submissões antecipadas;
	    resultados estarão disponíveis após <valid-from/>.</p> 
	<else/>    
	<ol class="submissions">
	  <submissions-list>
	  <li>
	    <a href="${report-url}"><submit-id/>&nbsp;<span class="${result-status}"><result-status/><valid-icon/></span></a>
	  </li>
	</submissions-list>
      </ol>
      </if-early>
      <else/>
      <p>Nenhuma submissão efetuada.</p>
    </if-submitted>
  </div>

  <div id="editor-tab" class="tabcontents">
    <page-title/>
    <apply template="_timing"/>
    <!-- <p class="info">Linguagens: <page-languages/>.</p>   -->
    <form id="codeform" method="POST" action="${page-url}">
      <p>
	<input type="file" id="fileselect"
	       accept="${language-extensions}"/> &nbsp;
	<input-language-selector id="langselect" name="language" form="codeform"/> &emsp;
	<input type="submit" value="Submeter"/> 
      </p>
      <p><textarea id="code" name="code" style="display:none;"/></p>
      <div id="editor"><default-text/></div>
      <!--
      <p>
	<input type="submit" value="Submeter"/> &emsp;
	<a href="${page-parent-url}"
	   class="button">Voltar à página de índice</a>
      </p>
      -->
    </form>
  </div>
</apply>


<script type="text/javascript">
  var editor = startAceEditor('editor');

  document.getElementById('codeform').addEventListener('submit',
    function() { submitListener(editor,'code'); });

  document.getElementById('fileselect').addEventListener('change',
    function() { fileListener(editor, 'fileselect', 'langselect'); });
					      
  document.getElementById('langselect').addEventListener('change',
    function() { editor.session.setMode(languageModes[this.selectedIndex]); } );


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

document.getElementById("description").click();
</script>

<language-constants-js/>
<default-language-js/>


