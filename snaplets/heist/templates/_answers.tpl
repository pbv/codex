<bind tag="boxcheck"><span style="font-size: 133%;">&#x2612;</span></bind>
<bind tag="boxuncheck"><span style="font-size: 133%;">&#x2610;</span></bind>
<bind tag="checkmark"><span style="color:green; font-size: 133%;">&#x2714;</span></bind>
<bind tag="crossmark"><span style="color:red; font-size: 133%;">&#x2718;</span></bind>
<bind tag="missmark"><span style="color:black; font-size: 133%;">&#x21e0;</span></bind>

<apply template="_base">
  <mathjax-js/>
  <div class="description">
    <quiz-preamble/>
  </div>

<h2>Submissão <submit-id/></h2>
<p>Enviada por <code><submit-user-id/></code> em <submit-time/>.</p>
<evaluating>
  <p><img src="/static/images/spinner.svg"></p>
  <p class="info">Se a página não atualizar automaticamente,
      use o botão "reload" do "browser".</p>
<else/>
      <h3>Resultado: <em><submit-classify/></em><overdue>&nbsp;(enviada fora do tempo)</overdue></h3>
      <feedback-high><pre><submit-message/></pre
      ></feedback-high>
  <form>
    <questions>
      <fieldset>
	<question-preamble/>
	<ol class="answers" type="${list-type}" start="${list-start}">
	  <alternatives>
	    <li><if-checked>
		<boxcheck/>&nbsp;
		<if-correct>
		  <alternative/>&ensp;<checkmark/>
		  <else/>
		  <span class="wrong"><alternative/></span>&ensp;<crossmark/>
		</if-correct>
		<else/>
		<boxuncheck/>&nbsp;
		<if-correct>
		  <span class="missing"><alternative/></span>&ensp;<missmark/>
		  <else/>
		  <alternative/> 
		</if-correct>
	      </if-checked>
	  </alternatives>
	</ol>
	<p><em>Answers</em>: <em><answers/></em></p>
      </fieldset>
    </questions>
    <p> <a href="${page-parent-url}"
	   class="button">Voltar à página de índice</a>
    </p>
  </form>
</evaluating>
</apply>

<apply template="_browse">
  <li><a title="Editar a página de exercício" href="${file-url}">Editar</a></li>
</apply>
