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
    <apply template="_editor"/>
  </div>

<script type="text/javascript" src="/static/js/tabview.js"/>
<script type="text/javascript" src="/static/js/ace-start.js"/>
<language-constants-js/>
<default-language-js/>
<script type="text/javascript">
document.getElementById("description").click();
</script>

</apply>
