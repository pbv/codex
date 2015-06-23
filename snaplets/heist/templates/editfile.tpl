<apply template="base">
<h2><edit_path/></h2>
<bind tag="submit_label">Gravar</bind>
<bind tag="post_action">/edit/${edit_path}</bind>
<bind tag="cancel_label">Cancelar</bind>
<bind tag="next_url">/problems</bind>
<apply template="_editor"><edit_source/></apply>
</apply>


