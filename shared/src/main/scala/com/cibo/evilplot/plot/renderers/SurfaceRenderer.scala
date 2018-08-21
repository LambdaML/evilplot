/*
 * Copyright (c) 2018, CiBO Technologies, Inc.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors
 *    may be used to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package com.cibo.evilplot.plot.renderers

import com.cibo.evilplot.colors._
import com.cibo.evilplot.geometry.{Drawable, EmptyDrawable, Extent, LineStyle, Path}
import com.cibo.evilplot.numeric.{Bounds, Point, Point3}
import com.cibo.evilplot.plot.aesthetics.{Theme, ThemedValue}
import com.cibo.evilplot.plot.renderers.SurfaceRenderer.SurfaceRenderContext
import com.cibo.evilplot.plot.{LegendContext, Plot}

trait SurfaceRenderer extends PlotElementRenderer[SurfaceRenderContext] {
  def legendContext(levels: Seq[Double])(implicit theme: Theme): LegendContext = LegendContext.empty
  def render(plot: Plot, extent: Extent, surface: SurfaceRenderContext)(
    implicit theme: Theme): Drawable
}

object SurfaceRenderer {

  /** The element renderer context for surface renderers. */
  case class SurfaceRenderContext(
    levels: Seq[Double],
    currentLevelPaths: Seq[Seq[Point]],
    currentLevel: Double)

  def contours(
    color: ThemedValue[Color] = (t: Theme) => t.colors.path,
    strokeWidth: ThemedValue[Double] = (t: Theme) => t.elements.strokeWidth,
    lineStyle: ThemedValue[LineStyle] = (t: Theme) => t.elements.lineDashStyle
  ): SurfaceRenderer = new SurfaceRenderer {
    def render(plot: Plot, extent: Extent, surface: SurfaceRenderContext)(
      implicit theme: Theme): Drawable = {
      surface.currentLevelPaths
        .map(
          pathpts =>
            Path(pathpts.map(p => Point(p.x, p.y)), strokeWidth)
              .dashed(lineStyle))
        .group
        .colored(color)
    }
  }

  def densityColorContours(points: Seq[Seq[Seq[Point3]]]): SurfaceRenderer =
    new SurfaceRenderer {
      private def getColorSeq(numPoints: Int): Seq[Color] =
        if (numPoints <= DefaultColors.lightPalette.length)
          DefaultColors.lightPalette.take(numPoints)
        else Color.stream.take(numPoints)

      def getBySafe[T](data: Seq[T])(f: T => Option[Double]): Option[Bounds] = {
        val mapped = data.map(f).filterNot(_.forall(_.isNaN)).flatten
        Bounds.get(mapped)
      }

      override def legendContext(levels: Seq[Double])(implicit theme: Theme): LegendContext = {
        val colors = getColorSeq(points.length)
        getBySafe(points)(_.headOption.flatMap(_.headOption.map(_.z)))
          .map { bs =>
            val bar = ScaledColorBar(colors, bs.min, bs.max)
            LegendContext.fromColorBar(bar)(theme)
          }
          .getOrElse(LegendContext.empty)
      }

      def render(plot: Plot, extent: Extent, surface: SurfaceRenderContext)(
        implicit theme: Theme): Drawable = {
        val surfaceRenderer = getBySafe(points)(_.headOption.flatMap(_.headOption.map(_.z)))
          .map { bs =>
            val bar = ScaledColorBar(getColorSeq(points.length), bs.min, bs.max)
            densityColorContours(bar)(points)
          }
          .getOrElse(contours())
        surfaceRenderer.render(plot, extent, surface)
      }
    }

  def densityColorContours(
    bar: ScaledColorBar
  )(points: Seq[Seq[Seq[Point3]]]): SurfaceRenderer = new SurfaceRenderer {
    def render(plot: Plot, extent: Extent, surface: SurfaceRenderContext)(
      implicit theme: Theme): Drawable = {
      surface.currentLevelPaths.headOption
        .map(pts =>
          contours(pts.headOption.fold(theme.colors.path)(_ => bar.getColor(surface.currentLevel)))
            .render(plot, extent, surface))
        .getOrElse(EmptyDrawable())
    }
  }

  def densityColorContours(
    coloring: ThemedValue[Coloring[Double]] = (t: Theme) => t.colors.continuousColoring,
    strokeWidth: ThemedValue[Double] = (t: Theme) => t.elements.strokeWidth,
    dashPattern: ThemedValue[LineStyle] = (t: Theme) => t.elements.lineDashStyle): SurfaceRenderer =
    new SurfaceRenderer {
      def render(plot: Plot, extent: Extent, surface: SurfaceRenderContext)(
        implicit theme: Theme): Drawable = {
        val color = coloring.get(theme).apply(surface.levels).apply(surface.currentLevel)
        surface.currentLevelPaths
          .map(
            pts =>
              contours(color, strokeWidth, dashPattern)
                .render(plot, extent, surface))
          .group
      }

      override def legendContext(levels: Seq[Double])(implicit theme: Theme): LegendContext = {
        coloring.legendContext(levels)
      }
    }
}
