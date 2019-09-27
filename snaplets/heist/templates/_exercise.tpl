<bind tag="icon-warning"><img src="/static/icons/16x16/warning.png"/></bind>
<bind tag="icon-editor"><img src="/static/icons/16x16/editor.png"/></bind>
<bind tag="valid-icon"><if-valid><else/><icon-warning/></if-valid></bind>

<apply template="_browse">
  <if-parent>
    <li><a href="${page-parent-url}" class="icon"
	   title="Voltar à página anterior">&curvearrowleft;</a></li>
  </if-parent>
  <li><button class="tablinks" onclick="openTab(event, 'description-tab')" id="description">Descrição</button></li>
  <li><button class="tablinks" onclick="openTab(event, 'editor-tab')">Editor</button></li>
  <li><button class="tablinks" onclick="openTab(event, 'submissions-tab')">Submissões</button><li>
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
	<if-submit-early>
	  <p><submissions-count/> submissões antecipadas;
	    resultados visíveis depois de <submit-after/>.</p> 
	  <else/>    
	  <ol class="submissions">
	    <submissions-list>
	      <li>
		<a href="${report-url}"><submit-id/>&nbsp;<span class="${result-status}"><result-status/></span></a>&nbsp;<valid-icon/>
	      </li>
	    </submissions-list>
	  </ol>
	</if-submit-early>
	<else/>
	<p>Nenhuma submissão efetuada.</p>
    </if-submitted>
  </div>

  <div id="editor-tab" class="tabcontents">
    <page-title/>
    <apply template="_timing"/>
    <if-allowed>
      <form id="codeform" method="POST" action="${page-url}">
	<p>
	  <input type="file" id="fileselect"
		 accept="${language-extensions}"/> &nbsp;
	  <input-language-selector id="langselect" name="language" form="codeform"/> &emsp;
	  <input type="submit" value="Submeter"/>
    &emsp;&emsp;&emsp;&emsp;&emsp;
    <apply template="_fontsize"/>
	</p>
	<p><textarea id="code" name="code" style="display:none;"/></p>
	<div id="editor"><default-text/></div>
      </form>
    </if-allowed>
  </div>
</apply>



<if-allowed>
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

document.getElementById("description").click();
</script>

