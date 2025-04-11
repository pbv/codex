<bind tag="valid-icon"><if-valid><else/>&#9888;</if-valid></bind>
<!-- -->
<apply template="_base">
  <mathjax-js/>
  <ace-editor-js/>
  <apply template="_browse">
    <if-parent>
      <li><a href="${page-parent-url}" 
	     title="Go back to parent"><apply template="_icon_back"/></a>
      </li>
    </if-parent>
    <li><button class="tablinks" onclick="openTab(event, 'description-tab')" id="description">Description</button></li>
    <li><button class="tablinks" onclick="openTab(event, 'editor-tab')">Editor</button></li>
    <li><button class="tablinks" onclick="openTab(event, 'submissions-tab')">Submissions</button><li>
  </apply>
  <div id="description-tab" class="tabcontents">
    <page-description/>
    <apply template="_timing"/>
  </div>
  <div id="submissions-tab" class="tabcontents">
    <page-title/>
    <p><submissions-count/> submissions.</p>
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
	  <input type="submit" value="Submit"/>
	  &emsp;&emsp;&emsp;&emsp;&emsp;
	  <apply template="_fontsize"/>
	</p>
	<p><textarea id="code" name="code" style="display:none;"/></p>
	<div id="editor"><default-text/></div>
      </form>  
      <else/>
      <p>Submissions closed.</p>
    </if-available>
  </div>

<script type="text/javascript" src="/static/js/tabview.js"/>
<script type="text/javascript" src="/static/js/ace-start.js"/>
<language-constants-js/>
<default-language-js/>
<script type="text/javascript">
document.getElementById("description").click();
</script>

</apply>
