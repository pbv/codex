<bind tag="icon-accepted"><img src="/icons/16x16/accepted.png"/></bind>
<bind tag="icon-rejected"><img src="/icons/16x16/rejected.png"/></bind>
<bind tag="icon-overdue"><img src="/icons/16x16/overdue.png"/></bind>
<bind tag="icon-editor"><img src="/icons/16x16/editor.png"/></bind>
<bind tag="submit-icons"><submit:overdue>&nbsp;<icon-overdue/><else/><submit:accepted>&nbsp;<icon-accepted/></submit:accepted></submit:overdue></bind>
<apply template="base">
<div class="description">
<page:description/>
</div>

<page:case-timing>
  <Early>
    <p>Submissões iniciam em: <validFrom/></p>
  </Early>
  <default>
    <p>Submissões terminam em: <validUntil/></p>
  </default>
</page:case-timing>

<if-submitted>
  <h2>Submissões anteriores</h2>
  <page:case-timing>
    <Early>
      <p><submissions-count/> submissões antecipadas; os resultados serão
	visíveis após <page:valid-from/>.</p>  
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
  </page:case-timing>
</if-submitted>

<h2>Nova submissão</h2>

<form id="editform" method="POST" action="${page:path}"
      onsubmit="submitAceEditorText('editform.editor');">
<p>
  <inputAceEditor id='editform.editor' mode='ace/mode/${page:language}'><page:code-text/></inputAceEditor>
</p>
<p>
  <input type="submit" value="Enviar"/>  &nbsp;
  <a href="${page:parent}" class="button">Voltar à folha de problemas</a>
</form>
</apply>

