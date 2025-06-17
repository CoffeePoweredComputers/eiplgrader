"""Factory for creating language adapters with backward compatibility."""

from typing import Dict, Type, Optional, Union, Any
from abc import ABC

from .base import LanguageAdapter, UnifiedLanguageAdapter, AdapterType
from .spec import LanguageSpec
from .validators import create_validator, ValidationStrategy


class AdapterFactory:
    """Factory for creating language adapters with unified interface."""

    def __init__(self):
        self._legacy_adapters: Dict[str, Type[LanguageAdapter]] = {}
        self._unified_adapters: Dict[str, Type[UnifiedLanguageAdapter]] = {}
        self._specs: Dict[str, LanguageSpec] = {}

    def register_legacy_adapter(
        self, name: str, adapter_class: Type[LanguageAdapter]
    ) -> None:
        """Register a legacy LanguageAdapter."""
        self._legacy_adapters[name] = adapter_class

    def register_unified_adapter(
        self, name: str, adapter_class: Type[UnifiedLanguageAdapter], spec: LanguageSpec
    ) -> None:
        """Register a UnifiedLanguageAdapter with its specification."""
        self._unified_adapters[name] = adapter_class
        self._specs[name] = spec

    def register_spec(self, name: str, spec: LanguageSpec) -> None:
        """Register a language specification."""
        self._specs[name] = spec

    def create_adapter(
        self,
        language: str,
        prefer_unified: bool = True,
        validation_strategy: Optional[str] = None,
        **kwargs,
    ) -> AdapterType:
        """Create an adapter for the specified language.

        Args:
            language: Language name
            prefer_unified: Whether to prefer unified adapters over legacy ones
            validation_strategy: Override validation strategy
            **kwargs: Additional arguments for adapter construction

        Returns:
            Language adapter instance

        Raises:
            ValueError: If language is not supported
        """
        # Try unified adapter first if preferred
        if prefer_unified and language in self._unified_adapters:
            adapter_class = self._unified_adapters[language]
            adapter = adapter_class(**kwargs)

            # Set up validation strategy if specified
            if validation_strategy and language in self._specs:
                spec = self._specs[language]
                spec.validation_strategy = validation_strategy
                validator = create_validator(spec)
                adapter.set_validation_strategy(validator)

            return adapter

        # Fall back to legacy adapter
        if language in self._legacy_adapters:
            legacy_adapter_class = self._legacy_adapters[language]
            return legacy_adapter_class(**kwargs)

        # If unified is available but not preferred, try it anyway
        if language in self._unified_adapters:
            unified_adapter_class = self._unified_adapters[language]
            return unified_adapter_class(**kwargs)

        raise ValueError(f"No adapter available for language: {language}")

    def get_available_languages(self) -> Dict[str, str]:
        """Get available languages and their types.

        Returns:
            Dictionary mapping language names to adapter types ('legacy' or 'unified')
        """
        languages = {}

        for name in self._legacy_adapters:
            languages[name] = "legacy"

        for name in self._unified_adapters:
            languages[name] = "unified"

        return languages

    def get_spec(self, language: str) -> Optional[LanguageSpec]:
        """Get language specification for a language."""
        return self._specs.get(language)

    def is_unified_adapter(self, language: str) -> bool:
        """Check if a language has a unified adapter."""
        return language in self._unified_adapters

    def is_legacy_adapter(self, language: str) -> bool:
        """Check if a language has a legacy adapter."""
        return language in self._legacy_adapters


class LegacyAdapterWrapper(UnifiedLanguageAdapter):
    """Wrapper to make legacy adapters compatible with unified interface."""

    def __init__(
        self, legacy_adapter: LanguageAdapter, spec: Optional[LanguageSpec] = None
    ):
        super().__init__()
        self._legacy_adapter = legacy_adapter
        self._spec = spec

        # Set up basic validation if spec is available
        if spec:
            validator = create_validator(spec)
            self.set_validation_strategy(validator)

    def get_spec(self) -> LanguageSpec:
        """Return language specification (may be minimal for legacy adapters)."""
        if self._spec:
            return self._spec

        # Create minimal spec from legacy config
        config = self._legacy_adapter.get_config()
        return LanguageSpec(
            name=config.name,
            display_name=config.display_name,
            file_extensions=config.file_extensions,
            run_command=config.run_command,
            docker_image=config.docker_image,
            compile_command=config.compile_command,
            test_timeout=config.test_timeout,
        )

    def _generate_prompt_impl(
        self,
        student_response: str,
        function_name: str,
        gen_type: str = "cgbg",
        **kwargs,
    ) -> str:
        """Delegate to legacy adapter."""
        return self._legacy_adapter.generate_prompt(
            student_response, function_name, gen_type, **kwargs
        )

    def _extract_code_impl(self, llm_response: str) -> list[str]:
        """Delegate to legacy adapter."""
        return self._legacy_adapter.extract_code(llm_response)

    def _validate_syntax_impl(self, code: str) -> tuple[bool, Optional[str]]:
        """Delegate to legacy adapter."""
        return self._legacy_adapter.validate_syntax(code)

    def _extract_functions_impl(self, code: str) -> list[Dict[str, Any]]:
        """Basic function extraction for legacy adapters."""
        # This is a simplified implementation
        # Real implementation would need language-specific parsing
        return [{"name": "unknown", "code": code, "signature": "unknown"}]

    def _normalize_code_impl(self, code: str) -> str:
        """Basic normalization for legacy adapters."""
        # Basic normalization - remove excess whitespace
        return code.strip()


class ComponentRegistry:
    """Registry for adapter components (validators, extractors, etc.)."""

    def __init__(self):
        self._validators: Dict[str, Type[ValidationStrategy]] = {}
        self._extractors: Dict[str, Any] = {}  # Type would be CodeExtractor
        self._normalizers: Dict[str, Any] = {}  # Type would be CodeNormalizer

    def register_validator(
        self, name: str, validator_class: Type[ValidationStrategy]
    ) -> None:
        """Register a validation strategy."""
        self._validators[name] = validator_class

    def get_validator(self, name: str) -> Optional[Type[ValidationStrategy]]:
        """Get a registered validator."""
        return self._validators.get(name)


# Global factory instance
_adapter_factory = AdapterFactory()
_component_registry = ComponentRegistry()


def get_adapter_factory() -> AdapterFactory:
    """Get the global adapter factory instance."""
    return _adapter_factory


def get_component_registry() -> ComponentRegistry:
    """Get the global component registry instance."""
    return _component_registry


def create_adapter(
    language: str,
    prefer_unified: bool = True,
    validation_strategy: Optional[str] = None,
    **kwargs,
) -> AdapterType:
    """Convenience function to create an adapter using the global factory."""
    return _adapter_factory.create_adapter(
        language, prefer_unified, validation_strategy, **kwargs
    )


def register_legacy_adapter(name: str, adapter_class: Type[LanguageAdapter]) -> None:
    """Convenience function to register a legacy adapter."""
    _adapter_factory.register_legacy_adapter(name, adapter_class)


def register_unified_adapter(
    name: str, adapter_class: Type[UnifiedLanguageAdapter], spec: LanguageSpec
) -> None:
    """Convenience function to register a unified adapter."""
    _adapter_factory.register_unified_adapter(name, adapter_class, spec)


def wrap_legacy_adapter(
    adapter: LanguageAdapter, spec: Optional[LanguageSpec] = None
) -> UnifiedLanguageAdapter:
    """Wrap a legacy adapter to provide unified interface."""
    return LegacyAdapterWrapper(adapter, spec)


def get_available_languages() -> Dict[str, str]:
    """Get all available languages and their adapter types."""
    return _adapter_factory.get_available_languages()


def migrate_legacy_adapters():
    """Auto-discover and register legacy adapters for backward compatibility.

    This function can be called during initialization to automatically
    register existing legacy adapters with the factory.
    """
    # This would scan the adapters directory and register found adapters
    # Implementation depends on the specific structure and naming conventions
    import importlib
    import pkgutil
    from . import adapters

    # Scan adapters package
    for _, module_name, _ in pkgutil.iter_modules(adapters.__path__):
        try:
            module = importlib.import_module(
                f".adapters.{module_name}", package=__package__
            )

            # Look for adapter classes
            for attr_name in dir(module):
                attr = getattr(module, attr_name)
                if (
                    isinstance(attr, type)
                    and issubclass(attr, LanguageAdapter)
                    and attr != LanguageAdapter
                    and attr != UnifiedLanguageAdapter
                ):

                    # Extract language name from class name (e.g., PythonAdapter -> python)
                    if attr_name.endswith("Adapter"):
                        lang_name = attr_name[:-7].lower()
                        register_legacy_adapter(lang_name, attr)

        except ImportError:
            # Skip modules that can't be imported
            continue
