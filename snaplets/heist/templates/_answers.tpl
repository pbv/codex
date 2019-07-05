<bind tag="boxcheck"><span style="font-size: 133%;">&#x2612;</span></bind>
<bind tag="boxuncheck"><span style="font-size: 133%;">&#x2610;</span></bind>
<bind tag="checkmark"><span style="color:green; font-size: 133%;">&#x2714;</span></bind>
<bind tag="crossmark"><span style="color:red; font-size: 133%;">&#x2718;</span></bind>
<bind tag="missmark"><span style="color:black; font-size: 133%;">&#x21e0;</span></bind>
<bind tag="spinner"><img src="/static/images/spinner.svg"></bind>

<apply template="_browse">
  <li><a href="${page-url}"
	 title="Voltar à página anterior">&curvearrowleft;</a></li>
</apply>


<apply template="_base">
  <mathjax-js/>
  
  <h2>Submissão <submit-id/></h2>
  <p>Enviada por <code><submit-user-id/></code> em <submit-time/>.</p>
  <if-evaluating>
    <p><spinner/></p>
    <p class="info">Se a página não atualizar automaticamente,
      use o botão "reload" do "browser".</p>
    <else/>
    <h3>Resultado: <em><result-status/></em><if-valid><else/>&nbsp;(<em><result-check/></em>)</if-valid></h3>
    <if-feedback>
      <pre><result-report/></pre>
      <questions>
	<fieldset>
	  <question-description/>
	  <question-fillin>
	    <p><question-answer/>&nbsp;<if-correct><checkmark/><else/><crossmark/></if-correct></p>
	    <if-correct><else/><p><em>Answer</em>: <em><question-answer-key/></em></p></if-correct>
	    <else/>
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
	<p><em>Answers</em>: <em><question-answer-key/></em></p>
	  </question-fillin>
      </fieldset>
      </questions>
    </if-feedback>
  </if-evaluating>
</apply>

