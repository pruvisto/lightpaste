/*
Language: Haskell
Author: Jeremy Hull <sourdrums@gmail.com>
*/

function(hljs) {
  var KEYWORDS = 'ML ML_command ML_file ML_prf ML_val abbreviation also apply apply_end arities assume atom_decl attribute_setup ax_specification axiomatization axioms back bnf_def boogie_end boogie_open boogie_status boogie_vc bundle by cannot_undo case cd chapter class class_deps classes classrel codata code_abort code_class code_const code_datatype code_deps code_include code_instance code_modulename code_monad code_pred code_reflect code_reserved code_thms code_type coinductive coinductive_set commit concrete_definition consts context corollary cpodef data datatype declaration declare def default_sort defer defer_recdef define_struct definition defs disable_pr display_drafts domain domain_isomorphism domaindef done enable_pr end enriched_type equivariance exit export_code extract extract_type finally find_consts find_theorems find_unused_assms fix fixrec from full_prf fun fun_cases function guess have header help hence hide_class hide_const hide_fact hide_type hoarestate import_const_map import_file import_tptp import_type_map include including inductive inductive_cases inductive_set inductive_simps init_toplevel instance instantiation interpret interpretation judgment kill kill_thy lemma lemmas let lift_definition linear_undo local_setup locale locale_deps method_setup moreover next nitpick nitpick_params no_notation no_syntax no_translations no_type_notation nominal_datatype nominal_inductive nominal_primrec nonterminal notation note notepad obtain oops oracle overloading parse_ast_translation parse_translation partial_function pcpodef pr prefer presume pretty_setmargin prf primrec print_abbrevs print_antiquotations print_ast_translation print_attributes print_binds print_bnfs print_bundles print_cases print_claset print_classes print_codeproc print_codesetup print_coercion_maps print_coercions print_commands print_configs print_context print_dependencies print_drafts print_facts print_induct_rules print_inductives print_interps print_locale print_locales print_methods print_orders print_quotconsts print_quotients print_quotmaps print_rules print_simpset print_statement print_syntax print_theorems print_theory print_trans_rules print_translation procedures proof prop pwd qed quickcheck quickcheck_generator quickcheck_params quit quotient_definition quotient_type realizability realizers recdef recdef_tc record refute refute_params remove_thy rep_datatype schematic_corollary schematic_lemma schematic_theorem sect section setup setup_lifting show simproc_setup sledgehammer sledgehammer_params smt_status solve_direct sorry spark_end spark_open spark_open_siv spark_open_vcg spark_proof_functions spark_status spark_types spark_vc specification statespace subclass sublocale subsect subsection subsubsect subsubsection syntax syntax_declaration term termination text text_raw then theorem theorems theory thm thm_deps thus thy_deps translations try txt txt_raw typ type_notation type_synonym typed_print_translation typedecl typedef ultimately undo undos_proof unfolding unused_thms use use_thy using value values verify_statement welcome with wrap_data write advanced and assumes attach avoids begin binder checking congs constrains datatypes defaults defines file fixes for functions hints identifier if imports in includes infix infixl infixr is keywords lazy module_name monos morphisms no_dests notes obtains open output overloaded permissive pervasive rep_compat shows structure unchecked unsafe uses where';
  
  var ML = {
    keywords:
    'abstype and andalso as case datatype do else end eqtype exception ' +
    'false fn fun functor handle if in include infix infixr let local ' +
    'NONE nonfix of op open orelse raise rec ref ' +
    'sharing sig signature SOME struct structure ' +
    'then true type val where while with withtype ML ' + 
    'THEN ORELSE APPEND THEN_ELSE THEN\' ORELSE\' APPEND\' DETERM COND TRY EVERY EVERY\' ' +
    'EVERY1 FIRST FIRST\' FIRST1 RANGE REPEAT_DETERM_N REPEAT_DETERM REPEAT ' +
    'REPEAT_DETERM1 DEPEAT1 FILTER CHANGED CHANGED_PROP ALLGOALS SOMEGOAL FIRSTGOAL ' +
    'HEADGOAL REPEAT_SOME REPEAT_DETERM_SOME REPEAT_FIRST REPEAT_DETERM_FIRST TRYALL ' +
    'CSUBGOAL SUBGOAL ASSERT_SUBGOAL CHANGED_GOAL SOLVED\' THEN_ALL_NEW REPEAT_ALL_NEW ' +
    'PRIMSEQ PRIMITIVE SINGLE CONVERSION RAW_METHOD_CASES RAW_METHOD METHOD_CASES ' +
    'METHOD SIMPLE_METHOD SIMPLE_METHOD\' SIMPLE_METHOD\'\'',
    begin: '\\bML *{\\*', end: '\\*}',
    relevance: 1,
    contains: [
      {
        className: 'comment',
        contains: ['self'],
        begin: '\\(\\*', end: '\\*\\)',
        relevance: 1
      },
      {
        className: 'preprocessor',
        contains: ['self'],
        begin: '@{', end: '}'
      },
      {
        className: 'string',
        begin: '#"', end: '"',
        illegal: '\\n',
        contains: [hljs.BACKSLASH_ESCAPE],
        relevance: 1
      },
      {
        className: 'keyword',
        begin: '#[1-9]\\d*',
        relevance: 0
      },
      {
        className: 'string',
        begin: '#"', end: '"',
        illegal: '\\n',
        contains: [hljs.BACKSLASH_ESCAPE],
        relevance: 1
      }, 
      {
        className: 'symbol',
        begin: ':|::|\\-\\-|\\|\\-\\-|\\^|\\@|\\+=|\\+\\+|\\+\\+=|\\+|\\-|\\*|/' +
               '|=|=\\>|\\-\\>|\\<|\\>|\\<\\>|\\>\\>|\\|\\>|\\b(o|oo|ooo|oooo)\\b',
        relevance: 0
      },
      hljs.QUOTE_STRING_MODE,
      {
        className: 'number',
        begin: '\\b~?(0[xX][a-fA-F0-9]+|(\\d+(\\.\\d*)?|\\.\\d+)([eE][~+]?\\d+)?)',
        relevance: 0
      }
    ]
  };
  
  var isa_basic = [
      {
        className: 'string',
        contains: ['self'],
        begin: '{\\*', end: '\\*}',
        relevance: 0
      },
      ML,
      {
        className: 'string',
        begin: '`', end: '`'
      },
      {
        className: 'comment',
        contains: ['self'],
        begin: '\\(\\*', end: '\\*\\)'
      },
      hljs.QUOTE_STRING_MODE,
      {
        className: 'number',
        begin: '\\b\\d+\\b',
        relevance: 0
      }
     ];
     
  
  return {
    keywords: KEYWORDS,
    contains: isa_basic.concat([
      {
        className: 'proof',
        keywords: KEYWORDS,
        contains: isa_basic.concat(['self', {
                              className: 'comment',
                              begin: '--', end: '$'
                           }]),
        begin: 'proof', end: 'qed|oops'
      }      
    ])
  };
}
