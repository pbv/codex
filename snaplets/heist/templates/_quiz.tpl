<apply template="_base">
  <mathjax-js/>
  <apply template="_browse">
    <if-parent>
      <li><a href="${page-parent-url}" class="icon"
	     title="Voltar à página anterior">&curvearrowleft;</a></li>    
    </if-parent>
    <!-- <ifAdmin>
      <li><a title="Editar a página" href="${file-url}">Editar</a></li>
    </ifAdmin> -->
  </apply>
    
  <div class="description">
    <quiz-preamble/>
  </div>
  <apply template="_timing"/>
  <if-available>
    <form id="quiz" method="POST" action="${page-url}">
      <p><input type="submit" value="Submeter"
	       onclick="quiz_modified=false;"/> &emsp;
	<input type="button" value="Limpar"
	       onclick="resetAll()"/></p>
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
      <p><input type="submit" value="Submeter"
	       onclick="quiz_modified=false;"/> &emsp;
	<input type="button" value="Limpar"
	       onclick="resetAll()"/></p>
    </form>
  </if-available>

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


