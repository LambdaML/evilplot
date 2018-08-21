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
import com.cibo.evilplot.geometry.{Disc, Drawable, EmptyDrawable, Extent}
import com.cibo.evilplot.plot.aesthetics.{Theme, ThemedValue}
import com.cibo.evilplot.plot.{LegendContext, Plot}

trait PointRenderer extends PlotElementRenderer[Int] {
  def legendContext(implicit theme: Theme): LegendContext = LegendContext()
  def render(plot: Plot, extent: Extent, index: Int)(implicit theme: Theme): Drawable
}

object PointRenderer {

  val defaultColorCount: Int = 10

  /** The default point renderer to render a disc.
    * @param color The color of the point.
    * @param pointSize The size of the point.
    * @param label Label to be shown in a legend.
    */
  def default(
    color: ThemedValue[Color] = (t: Theme) => t.colors.point,
    pointSize: ThemedValue[Double] = (t: Theme) => t.elements.pointSize,
    label: Drawable = EmptyDrawable()
  )(implicit theme: Theme): PointRenderer = new PointRenderer {
    override def legendContext(implicit theme: Theme): LegendContext = label match {
      case _: EmptyDrawable => LegendContext.empty
      case d =>
        LegendContext.single(Disc.centered(pointSize).filled(color), d)
    }
    def render(plot: Plot, extent: Extent, index: Int)(implicit theme: Theme): Drawable = {
      Disc.centered(pointSize).filled(color)
    }
  }

  /**
    * Render points with colors based on a third, continuous variable.
    * @param depths The depths for each point.
    * @param coloring The coloring to use.
    * @param size The size of the point.
    */
  def depthColor(
    depths: Seq[Double],
    coloring: ThemedValue[Coloring[Double]] = (t: Theme) => t.colors.continuousColoring,
    size: ThemedValue[Double] = (t: Theme) => t.elements.pointSize
  )(implicit theme: Theme): PointRenderer = new PointRenderer {

    def render(plot: Plot, extent: Extent, index: Int)(implicit theme: Theme): Drawable = {
      // TODO: don't need to rerun the coloring thing each time
      Disc.centered(size).filled(coloring.get(theme).apply(depths)(theme)(depths(index)))
    }

    override def legendContext(implicit theme: Theme): LegendContext =
      coloring.legendContext(depths)
  }

  /**
    * Render points with colors based on a third, categorical variable.
    * @param colorDimension Categories for each point.
    * @param coloring The coloring to use. If not provided, one based on the
    *                 color stream from the theme is used.
    * @param size The size of the points.
    * @tparam A the type of the categorical variable.
    */
  def colorByCategory[A: Ordering](
    colorDimension: Seq[A],
    coloring: ThemedValue[Coloring[A]] = CategoricalColoring.themed[A],
    size: ThemedValue[Double] = (t: Theme) => t.elements.pointSize
  )(implicit theme: Theme): PointRenderer = new PointRenderer {

    def render(plot: Plot, extent: Extent, index: Int)(implicit theme: Theme): Drawable = {
      Disc
        .centered(size)
        .filled(coloring.get(theme).apply(colorDimension)(theme)(colorDimension(index)))
    }

    override def legendContext(implicit theme: Theme): LegendContext =
      coloring.legendContext(colorDimension)
  }

  /**
    * A no-op renderer for when you don't want to render points (such as on a line)
    */
  def empty(): PointRenderer = new PointRenderer {
    def render(plot: Plot, extent: Extent, index: Int)(implicit theme: Theme): Drawable =
      EmptyDrawable()
  }
}
