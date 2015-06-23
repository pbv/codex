<apply template="base">
<h1>Problem <problem_id/></h1>
<h2><edit_path/></h2>
<bind tag="submit_label">Gravar</bind>
<bind tag="post_action">/edit/${edit_path}?pid=${problem_id}</bind>
<bind tag="cancel_label">Cancelar</bind>
<bind tag="next_url">/problems/${problem_id}</bind>
<apply template="_editor"><edit_source/></apply>
</apply>


