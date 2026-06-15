exemplo_card <- function(inputId, titulo, descricao = "") {
  div(
    style = "
      padding: 12px;
      border: 1px solid #d4d4d4;
      border-radius: 10px;
      margin-bottom: 12px;
      cursor: pointer;
      background-color: #fafafa;
    ",
    actionLink(
      inputId = inputId,  # 👈 agora recebe ID pronto
      label = HTML(paste0(
        "<b>", titulo, "</b><br>",
        "<span style='font-size:14px;'>", descricao, "</span>"
      )),
      style = "color:#003366;"
    )
  )
}