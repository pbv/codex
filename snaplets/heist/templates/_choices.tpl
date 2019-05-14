<ol class="answers" type="${list-type}" start="${list-start}">
  <alternatives>
    <li>
      <label>
	<if-checked>
	  <input type="checkbox" name="${question-name}" value="${alternative-label}" onclick="quiz_modified=true;${onclick-callback}" checked/>
	  <else/>
	  <input type="checkbox" name="${question-name}" value="${alternative-label}" onclick="quiz_modified=true;${onclick-callback}"/>
	</if-checked>&nbsp;<alternative/>
      </label>
    </li>
  </alternatives>
</ol>
