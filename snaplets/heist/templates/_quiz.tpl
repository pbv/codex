<apply template="_base">
  <mathjax-js/>
  <div class="description">
    <quiz-preamble/>
  </div>

  <apply template="_timing"/>
  
  <form id="quiz" method="POST" action="${page-url}">
    <questions>
      <fieldset>
	<question-description/>
	<question-fillin>
	  <apply template="_fillin"/>
	  <else/>
	  <apply template="_choices"/>	  
	</question-fillin>
      </fieldset>
    </questions>
    <div>
      <p><input type="submit" value="Submeter" onclick="quiz_modified=false;"/> &emsp;
	<a href="${page-parent-url}"
	   class="button">Voltar à página de índice</a> &emsp;
	<input type="button" value="Limpar seleção" style="float:right"
	       onclick="resetAll()"/>
      </p>
    </div>
  </form>

  <script>
    var quiz_modified = false;
    
    function onlyOne(checkbox) {
    var array = document.forms["quiz"].getElementsByTagName("input");
    for (i = 0; i<array.length; i++) {
	var item= array[i];
	if (item !== checkbox && item.name == checkbox.name) item.checked = false
     }
    }

   function resetAll() {
    quiz_modified = true;				
    var array = document.forms["quiz"].getElementsByTagName("input");
    for (i = 0; i<array.length; i++) {
	var item= array[i];
	if (item.type == "checkbox") item.checked = false
    }
   }

   window.onbeforeunload = function(e) {
     if (quiz_modified) {
	return "Confirm?";
     } else {
       return;
     }
    }; 
   
  </script>
</apply>


<apply template="_browse">
  <li><a title="Editar a página de exercício" href="${file-url}">Editar</a></li>
</apply>
