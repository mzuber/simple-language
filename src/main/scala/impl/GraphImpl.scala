/*
 * Copyright (c) 2012, TU Berlin
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *   * Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *   * Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *   * Neither the name of the TU Berlin nor the
 *     names of its contributors may be used to endorse or promote products
 *     derived from this software without specific prior written permission.

 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL TU Berlin BE LIABLE FOR ANY
 * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */

package de.tuberlin.uebb.sl2.impl

import org.jgrapht.{DirectedGraph}
import org.jgrapht.graph.{DefaultDirectedGraph, DefaultEdge}
import org.jgrapht.traverse.{TopologicalOrderIterator}
import org.jgrapht.alg.{StrongConnectivityInspector}

import de.tuberlin.uebb.sl2.modules.Graph

import scala.collection.JavaConversions._

/**
  * Wrapper for the JGraphT library
  */
trait GraphImpl[T] extends Graph[T]{

  type Graph = DirectedGraph[T, DefaultEdge]


  /**
    * Construct a directed graph from a list of vertices and a list of edges.
    */
  def directedGraph(vertices: Set[T], edges: List[(T,T)]): Graph = {
    val graph = new DefaultDirectedGraph[T, DefaultEdge](classOf[DefaultEdge])

    // add all vertices
    for (vertex <- vertices) {
      graph.addVertex(vertex)
    }

    // add all edges
    for ( (from, to) <- edges ) {
      graph.addEdge(from, to)
    }

    graph
  }

  
  /**
    * Determine the strongly connected components of the given graph.
    */
  def stronglyConnectedComponents(graph: Graph): Map[T, Set[T]] = {

    import scala.collection.JavaConversions._

    val sccs = new StrongConnectivityInspector(graph).stronglyConnectedSets() map (Set() ++ _)
    
    Map() ++ (for (scc <- sccs ; v <- scc) yield (v -> scc))
  }


  /**
    * Sort a list of strongly connected components in topological order.
    *
    * @param sccs     Strongly connected components
    * param baseGraph The graph the strongly connected components stem from
    */
  def topologicalSort(sccs: Map[T, Set[T]], baseGraph: Graph): List[Set[T]] = {
    
    val graph = new DefaultDirectedGraph[Set[T], DefaultEdge](classOf[DefaultEdge])

    for(scc <- sccs.values) {
      graph.addVertex(scc)
    }

    for(e <- baseGraph.edgeSet) {
      val src = sccs(baseGraph.getEdgeTarget(e))
      val trgt = sccs(baseGraph.getEdgeSource(e))
      if (src != trgt)
        graph.addEdge(src, trgt)
    }

    val iter = new TopologicalOrderIterator[Set[T], DefaultEdge](graph)

    List() ++ iter
  }
}
