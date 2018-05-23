<bind tag="icon-accepted"><img src="/static/icons/16x16/accepted.png"/></bind>
<bind tag="icon-rejected"><img src="/static/icons/16x16/rejected.png"/></bind>
<bind tag="icon-overdue"><img src="/static/icons/16x16/overdue.png"/></bind>
<bind tag="icon-editor"><img src="/static/icons/16x16/editor.png"/></bind>

<apply template="_base">
<h1><page-title/></h1>
<h2>Submissão <submit-id/></h2>
<p>Enviada por <code><submit-user-id/></code> em <submit-time/>.</p>
<evaluating>
  <p><img src="/static/images/spinner.svg"></p>
  <p class="info">Se a página não atualizar automaticamente,
      use o botão "reload" do "browser".</p>
<else/>
<feedback-medium>
  <current-timing>
    <Early>
      <p>Submissão antecipada; o resultado ficará visível após <valid-from/>.</p>
    </Early>
    <default>
      <h3>Resultado: <em><submit-classify/></em><overdue>&nbsp;(enviada fora do tempo)</overdue></h3>
      <feedback-high><pre><submit-message/></pre
      ></feedback-high>
      </default>
  </current-timing>
</feedback-medium>

<h2>Editar submissão</h2>

<form id="editform" method="POST" action="${page-url}"
      onSubmit="submitAceEditorText('submission');">
<p><inputAceEditor id="submission" path="${language-ext}"><code-text/></inputAceEditor></p>
<p><input type="submit" value="Submeter novamente"/>
&nbsp;<a class="button" href="${page-url}">Voltar ao exercício</a>
&nbsp;<a class="button" href="${page-parent-url}">Voltar à folha de exercícios</a>
</form>
</evaluating>
</apply>

<apply template="_browse">
  <li><a title="Editar folha de exercício"
     href="${file-url}">Editar</a><li>
</apply>
