// -*- Mode: C++; -*-
// TinyCl - DOM - Object Layout
// dom/dom_layout.inc
//
// Copyright (C) 2007-2008 by Project Vogue.
// Written by Yoshifumi "VOGUE" INOUE. (yosi@msn.com)
//
// @(#)$Id: //proj/evedit2/mainline/tinycl/tinycl_layout.inc#35 $
//
// XML DOM
//  expanded-name
//  iterator
//      node-iterator
//          named-node-map
//          node-list
//  name-object
//  namespace
//  node
//      character-data
//          cdata-section
//          comment
//          text
//      entity-reference
//      parent-node
//          document
//          document-fragment
//          floating-node
//          named-node
//              attribute
//              element
//      processing-instruction
//      xml-declaration
//
// References:
//  [1] http://www.w3.org/TR/2004/REC-DOM-Level-3-Core-20040407/
//  [2] http://www.w3.org/TR/REC-DOM-Level-1/
//  [3] http://www.w3.org/TR/2006/REC-xml-names-20060816/
//
deflayout(dom_node_iterator, structure, : Layout_structure_object)
    direct_super_(structure_object)
    direct_slot_(dom_node,           Knode,          or_dom_node_null)
    direct_slot_(dom_parent_node,    Kparent_node,   dom_parent_node)
endlayout(dom_node_iterator)

defabstract(dom_node, structure, : Layout_structure_object)
    direct_super_(structure_object)
    direct_slot_(dom_flags,                 // [1]
        nil, fixnum )
    direct_slot_(dom_next_sibling,          // [2]
        nil, or_dom_node_null )
    direct_slot_(dom_parent_node,           // [3]
        Kparent_node, dom_parent_node )
    direct_slot_(dom_previous_sibling,      // [4]
        nil, or_dom_node_null )
endabstract(dom_node)

defabstract(dom_character_data, structure, : Layout_dom_node)
    direct_super_(dom_node)
    direct_slot_(dom_data,  Kdata, string)  // [5]
endabstract(dom_character_data)

defabstract(dom_parent_node, structure, : Layout_dom_node)
    direct_super_(dom_node)
    direct_slot_(dom_first_child,    nil, or_dom_node_null) // [5]
    direct_slot_(dom_last_child,     nil, or_dom_node_null) // [6]
endabstract(dom_parent_node)

defabstract(dom_named_node, structure, : Layout_dom_parent_node)
    direct_super_(dom_parent_node)
    direct_slot_(dom_name,   Kname,   dom_name_object)   // [7]
endabstract(dom_named_node)

deflayout(dom_attribute, structure, : Layout_dom_named_node)
    direct_super_(dom_named_node)
endlayout(dom_attribute)

deflayout(dom_cdata_section, structure, : Layout_dom_character_data)
    direct_super_(dom_character_data)
endlayout(dom_cdata_section)

deflayout(dom_comment, structure, : Layout_dom_character_data)
    direct_super_(dom_character_data)
endlayout(dom_comment)

deflayout(dom_document, structure, : Layout_dom_parent_node)
    direct_super_(dom_parent_node)
    direct_slot_(dom_floating_node, nil, dom_floating_node)
endlayout(dom_document)

deflayout(dom_document_fragment, structure, : Layout_dom_parent_node)
    direct_super_(dom_parent_node)
endlayout(dom_document_fragment)

deflayout(dom_element, structure, : Layout_dom_named_node)
    direct_super_(dom_named_node)
endlayout(dom_element)

deflayout(dom_entity_reference, structure, : Layout_dom_node)
    direct_super_(dom_node)
endlayout(dom_entity_reference)

deflayout(dom_expanded_name, structure, : Layout_structure_object)
    direct_super_(structure_object)
    direct_slot_(dom_local_name, nil, string)
    direct_slot_(dom_namespace,  nil, dom_namespace)
endlayout(dom_expanded_name)

deflayout(dom_floating_node, structure, : Layout_dom_parent_node)
    direct_super_(dom_parent_node)
endlayout(dom_floating_node)

deflayout(dom_named_node_map, structure, : Layout_dom_node_iterator)
    direct_super_(dom_node_iterator)
endlayout(dom_named_node_map)

deflayout(dom_namespace, structure, : Layout_structure_object)
    direct_super_(structure_object)
    direct_slot_(dom_expanded_table,    nil, hash_table)
    direct_slot_(dom_qualified_table,   nil, hash_table)
    direct_slot_(uri,                   nil, string)
endlayout(dom_namespace)

deflayout(dom_node_list, structure, : Layout_dom_node_iterator)
    direct_super_(dom_node_iterator)
endlayout(dom_node_list)

deflayout(dom_name_object, structure, : Layout_structure_object)
    direct_super_(structure_object)
    direct_slot_(dom_expanded_name,  nil, or_dom_expanded_name_null)
    direct_slot_(dom_qualified_name, nil, string)
endlayout(dom_name_object)

deflayout(dom_text, structure, : Layout_dom_character_data)
    direct_super_(dom_character_data)
endlayout(dom_text)

defabstract(dom_dom_error, standard, : Layout_error)
    direct_super_(error)
endabstract(dom_dom_error)

defstorage(dom_hierachy_error, standard, : Layout_dom_dom_error)
    direct_super_(dom_dom_error)
    direct_slot_(dom_node,          Knode,          dom_node)
    direct_slot_(dom_parent_node,   Kparent_node,   dom_node)
endstorage(dom_hierachy_error)

defstorage(dom_invalid_character, standard, : Layout_dom_dom_error)
    direct_super_(dom_dom_error)
endstorage(dom_invalid_character)

defstorage(dom_invalid_modification, standard, : Layout_dom_dom_error)
    direct_super_(dom_dom_error)
endstorage(dom_invalid_modification)

defstorage(dom_namespace_error, standard, : Layout_dom_dom_error)
    direct_super_(dom_dom_error)
endstorage(dom_namespace_error)

defstorage(dom_no_modification_allowed, standard, : Layout_dom_dom_error)
    direct_super_(dom_dom_error)
    direct_slot_(dom_node,      Knode,      dom_node)
endstorage(dom_no_modification_allowed)

defstorage(dom_not_found, standard, : Layout_dom_dom_error)
    direct_super_(dom_dom_error)
endstorage(dom_not_found)

defstorage(dom_wrong_document, standard, : Layout_dom_dom_error)
    direct_super_(dom_dom_error)
    direct_slot_(dom_document,  Kdocument,  dom_document)
    direct_slot_(dom_node,      Knode,      dom_node)
endstorage(dom_wrong_document)

// EOF
