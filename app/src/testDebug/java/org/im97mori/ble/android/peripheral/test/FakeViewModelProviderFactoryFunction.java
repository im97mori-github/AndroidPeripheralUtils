package org.im97mori.ble.android.peripheral.test;

import androidx.core.util.Pair;
import androidx.lifecycle.HasDefaultViewModelProviderFactory;
import androidx.lifecycle.ViewModel;
import androidx.lifecycle.ViewModelProvider;
import androidx.lifecycle.viewmodel.CreationExtras;
import kotlin.reflect.KClass;
import org.jetbrains.annotations.NotNull;

import java.util.HashSet;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;

import static kotlin.jvm.JvmClassMappingKt.getJavaClass;

public class FakeViewModelProviderFactoryFunction implements Function<HasDefaultViewModelProviderFactory, ViewModelProvider.Factory> {

    private final Set<Pair<Class<? extends ViewModel>, Class<? extends ViewModel>>> set = new HashSet<>();

    public <T extends ViewModel, T2 extends T> void setFakeViewModelClass(Class<T> t, Class<T2> t2) {
        set.add(Pair.create(t, t2));
    }

    public <T extends ViewModel, T2 extends T> Class<T> get(Class<T> t) {
        Optional<? extends Class<? extends ViewModel>> optional = set.stream()
                .filter(it -> it.first.equals(t))
                .map(it -> it.second)
                .findAny();
        if (optional.isPresent()) {
            //noinspection unchecked
            return (Class<T>) optional.get();
        } else {
            return t;
        }
    }

    @Override
    public ViewModelProvider.Factory apply(HasDefaultViewModelProviderFactory hasDefaultViewModelProviderFactory) {
        ViewModelProvider.Factory originalFactory = hasDefaultViewModelProviderFactory.getDefaultViewModelProviderFactory();
        return new ViewModelProvider.Factory() {
            @Override
            public @NotNull <T extends ViewModel> T create(@NotNull KClass<T> modelClass, @NotNull CreationExtras extras) {
                return originalFactory.create(get(getJavaClass(modelClass)), extras);
            }

            @Override
            public @NotNull <T extends ViewModel> T create(@NotNull Class<T> modelClass, @NotNull CreationExtras extras) {
                return originalFactory.create(get(modelClass), extras);
            }

            @Override
            public @NotNull <T extends ViewModel> T create(@NotNull Class<T> modelClass) {
                return originalFactory.create(get(modelClass));
            }
        };
    }
}
