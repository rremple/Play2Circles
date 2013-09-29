package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import play.api.data.validation.Constraints._

import views._

import models._

object Vennomatic extends Controller {

  /**
   * Venn-o-matic Form definition.
   */
  val vennomaticForm = Form[VennDiagramParameters](
    mapping(
      "a" -> number(min = 0),
      "b" -> number(min = 0),
      "c" -> number(min = 0),
      "ab" -> number(min = 0),
      "bc" -> number(min = 0),
      "ca" -> number(min = 0)
    )(VennDiagramParameters.apply)(VennDiagramParameters.unapply))

  /**
   * Display a form with default values and no diagram.
   */
  def form = Action {
    val defaultParams = VennDiagramParameters(314, 314, 314, 100, 100, 100)
    Ok(html.vennomatic(vennomaticForm.fill(defaultParams), Right(None)));
  }

  /**
   * Handle form submission, draw diagram if we can.
   */
  def submit = Action { implicit request =>
    vennomaticForm.bindFromRequest.fold(
      errors => BadRequest(html.vennomatic(errors, Left(List("Bad request")))), // e.g., field invalid, like circle size<0
      params => validateAndDisplayDiagram(params) // no form errors
    )
  }

  private def validateAndDisplayDiagram(params: VennDiagramParameters) = {
    params.errors match {
      case List() => displayDiagram(params) // no validation errors
      case _ => BadRequest(html.vennomatic(vennomaticForm.fill(params), Left(params.errors))) // e.g., intersection > circle size
    }
  }

  private def displayDiagram(params: VennDiagramParameters) = {
    try { 
      Ok(html.vennomatic(vennomaticForm.fill(params), Right(Some(VennDiagram(params))))) // no calculation errors for new diagram
    } catch {
      case e: RuntimeException => 
        BadRequest(html.vennomatic(vennomaticForm.fill(params), Left(List(e.getMessage)))) // e.g. inconsistent geometry
    }
  }

}