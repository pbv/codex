<bind tag="icon-warning">&#9888;</bind>
<bind tag="valid-icon"><if-valid><else/><icon-warning/></if-valid></bind>
<!--  -->
<apply template="_base">
<mathjax-js/>
<ace-editor-js/>
<apply template="_browse">
  <li><a href="${page-url}" class="icon"
	 title="Voltar à página anterior">&curvearrowleft;</a></li>
  <li><button class="tablinks" onclick="openTab(event, 'report-tab')" id="report">Relatório</button></li>
  <li><button class="tablinks" onclick="openTab(event, 'editor-tab')">Editor</button></li>
</apply>

<div id="report-tab" class="tabcontents">
  <page-title/>
  <h2>Submissão <submit-id/></h2>
  <p>Enviada por <code><submit-user-id/></code> em <submit-time/>.</p>
  <if-evaluating>
    <p><img src="/static/images/spinner.svg"></p>
    <p class="info">Se a página não atualizar automaticamente,
      use o botão "reload" do "browser".</p>
    <else/>
    <h3>Resultado: <em><result-status/></em></h3>
    <if-valid><else/><p><icon-warning/> Submissão inválida: <em><invalid-msg/></em></p></if-valid>
    <if-show-feedback>
      <pre><result-report/></pre>
      <if-pythontutor>
	<p><a href="${pythontutor-url}" class="button"
	      target="_blank">Executar usando o <em>PythonTutor</em></a><p>
      </if-pythontutor>
    </if-show-feedback>
  </if-evaluating>
</div>

<div id="editor-tab" class="tabcontents">
  <apply template="_editor"/>
</div>

<script type="text/javascript" src="/static/js/tabview.js"/>
<script type="text/javascript" src="/static/js/ace-start.js"/>
<language-constants-js/>
<default-language-js/>
<script type="text/javascript">
  document.getElementById("report").click();
</script>
</apply>
  

