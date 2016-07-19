<bind tag="icon-accepted"><img src="/icons/16x16/accepted.png"/></bind>
<bind tag="icon-rejected"><img src="/icons/16x16/rejected.png"/></bind>
<bind tag="icon-overdue"><img src="/icons/16x16/overdue.png"/></bind>
<bind tag="icon-editor"><img src="/icons/16x16/editor.png"/></bind>
<bind tag="submit-icons"><submit:overdue>&nbsp;<icon-overdue/><else/><submit:accepted>&nbsp;<icon-accepted/></submit:accepted></submit:overdue></bind>
<apply template="base">
<div class="description">
<page:description/>
</div>

<exercise:timing>
  <Early>
    <p>Submissões iniciam em: <exercise:valid-from/></p>
  </Early>
  <Valid>
    <p>Submissões terminam em: <exercise:valid-until/></p>
  </Valid>
  <Overdue>
    <p>Submissões terminaram em: <exercise:valid-until/></p>
  </Overdue>
</exercise:timing>

<if-submitted>
  <h2>Submissões anteriores</h2>
  <exercise:timing>
    <Early>
      <p><submissions-count/> submissões antecipadas; os resultados serão
	visíveis após <exercise:valid-from/>.</p>  
    </Early>
    <default>
      <ol class="submissions">
	<submissions-list>
	  <li>
	    <a href="/submit/${submit:id}">
	      <span class="info"><submit:classify/></span></a>
	  </li>
	</submissions-list>
      </ol>
    </default>
  </exercise:timing>
</if-submitted>

<h2>Nova submissão</h2>

<form id="editform" method="POST" action="${page:path}"
      onsubmit="submitAceEditorText('editform.editor');">
<p>
  <inputAceEditor id='editform.editor' mode='ace/mode/${exercise:language}'><exercise:code-text/></inputAceEditor>
</p>
<p>
  <input type="submit" value="Enviar"/>  &nbsp;
  <a href="${page:parent}" class="button">Voltar à folha de problemas</a>
</form>
</apply>

