<bind tag="icon_accepted"><img src="/icons/16x16/accepted.png"/></bind>
<bind tag="icon_rejected"><img src="/icons/16x16/rejected.png"/></bind>
<bind tag="icon_overdue"><img src="/icons/16x16/overdue.png"/></bind>
<bind tag="icon_editor"><img src="/icons/16x16/editor.png"/></bind>
<bind tag="submit_icons"><submitOverdue>&nbsp;<icon_overdue/><else/><submitAccepted>&nbsp;<icon_accepted/></submitAccepted></submitOverdue></bind>
<apply template="base">
<div class="description">
<pageDescription/>
</div>

<exerciseTiming>
  <Early>
    <p>Submissões iniciam em: <validFrom/></p>
  </Early>
  <default>
    <p>Submissões terminam em: <validUntil/></p>
  </default>
</exerciseTiming>

<ifSubmitted>
  <h2>Submissões anteriores</h2>
  <exerciseTiming>
    <Early>
      <p><submissionsCount/> submissões antecipadas; os resultados serão
	visíveis após <validFrom/>.</p>  
    </Early>
    <default>
      <ol class="submissions">
	<submissionList>
	  <li>
	    <a href="/submit/${submitID}">
	      <span class="info"><submitClassify/></span></a>
	  </li>
	</submissionList>
      </ol>
    </default>
  </exerciseTiming>
</ifSubmitted>

<h2>Nova submissão</h2>

<form id="editform" method="POST" action="${pagePath}"
      onsubmit="submitAceEditorText('editform.editor');">
<p>
  <inputAceEditor id='editform.editor' mode='ace/mode/${pageLanguage}'><pageCodeText/></inputAceEditor>
</p>
<p>
  <input type="submit" value="Enviar"/>  &nbsp;
  <a href="${pageParent}" class="button">Voltar à folha de problemas</a>
</form>
</apply>

