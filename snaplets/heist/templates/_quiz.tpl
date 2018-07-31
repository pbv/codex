
<apply template="_base">
  <mathjax-js/>
  <div class="description">
    <quiz-preamble/>
  </div>
  
  <form id="quiz" method="POST" action="${quiz-url}">
    <questions>
      <fieldset>
      <answer-preamble/>
      <ol type="${list-type}" start="${list-start}">
	<answers>
	  <li><input type="checkbox" name="${answer-name}" value="${answer-label}"/> <answer-item/>
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
