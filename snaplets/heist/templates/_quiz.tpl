
<apply template="_base">
  <mathjax-js/>
  <div class="description">
    <quiz-preamble/>
  </div>
  
  <form id="quiz" method="POST" action="${quiz-url}">
    <questions>
      <fieldset>
      <answer-preamble/>
      <ol class="answers" type="${list-type}" start="${list-start}">
	<answers>
	  <li><label><input type="checkbox" name="${answer-name}" value="${answer-label}"/>&nbsp;<answer-item/></label>
	  </li>
	</answers>
      </ol>
      </fieldset>
    </questions>
  </form>
</apply>


<apply template="_browse">
  <li><a title="Editar a página de exercício" href="${file-url}">Editar</a></li>
</apply>
