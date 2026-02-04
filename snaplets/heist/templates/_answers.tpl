<bind tag="icon-warning">&#9888;</bind>
<bind tag="boxcheck"><span style="font-size: 133%;">&#x2612;</span></bind>
<bind tag="boxuncheck"><span style="font-size: 133%;">&#x2610;</span></bind>
<bind tag="checkmark"><span style="color:green; font-size: 133%;">&#x2714;</span></bind>
<bind tag="crossmark"><span style="color:red; font-size: 133%;">&#x2718;</span></bind>
<bind tag="missmark"><span style="color:black; font-size: 133%;">&#x21e0;</span></bind>
<bind tag="spinner"><img src="/static/images/spinner.svg"></bind>

<apply template="_base">
  <mathjax-js/>
  <apply template="_browse">
    <li><a href="${page-url}"
	   title="Back to the parent page"><apply template="_icon_back"/></a></li>
  <li><apply template="_changelang"/></li>
  </apply>
  
  <h2>Submission <submit-id/></h2>
  <p>Submited by <code><submit-user-id/></code> in <submit-time/>.</p>
  <if-evaluating>
    <p><spinner/></p>
    <p class="info">Se a página não atualizar automaticamente,
      use o botão "reload" do "browser".</p>
    <else/>
    <h3>Result: <em><result-status/></em></h3>
    <if-valid><else/><icon-warning/>Invalid submission: <em><invalid-msg/></em></if-valid>
    <if-show-feedback>
      <quiz-report-summary/>
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
    </if-show-feedback>
  </if-evaluating>
<script type="text/javascript" src="/static/js/changelang.js"/>
</apply>

