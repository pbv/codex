
<apply template="_browse">
  <if-parent>
    <li><a href="${page-parent-url}"
	 title="Voltar à página anterior">&curvearrowleft;</a></li>    
  </if-parent>
  <!-- 
  <li><button class="tablinks"
	      value="Submeter"
	      onclick="quiz_modified=false;document.forms['quiz'].submit()">Submeter</button></li>
  <li><button class="tablinks" value="Limpar"
	      onclick="resetAll()">Limpar</button></li>
  -->
  <ifAdmin>
    <li><a title="Editar a página" href="${file-url}">Editar</a></li>
  </ifAdmin>
</apply>


<apply template="_base">
  <mathjax-js/>
    
  <div class="description">
    <quiz-preamble/>
  </div>
  <apply template="_timing"/>
  
  <form id="quiz" method="POST" action="${page-url}">
  <p>
    <input type="submit" value="Submeter"
	   onclick="quiz_modified=false;"/> &emsp;
    <input type="button" value="Limpar"
	   onclick="resetAll()"/>
  </p>
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


