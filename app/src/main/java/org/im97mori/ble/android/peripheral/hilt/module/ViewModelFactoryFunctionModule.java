package org.im97mori.ble.android.peripheral.hilt.module;

import androidx.lifecycle.HasDefaultViewModelProviderFactory;
import androidx.lifecycle.ViewModelProvider;
import dagger.Module;
import dagger.Provides;
import dagger.hilt.InstallIn;
import dagger.hilt.components.SingletonComponent;

import javax.inject.Singleton;
import java.util.function.Function;

@Module
@InstallIn(SingletonComponent.class)
public class ViewModelFactoryFunctionModule {

    @Singleton
    @Provides
    public static Function<HasDefaultViewModelProviderFactory, ViewModelProvider.Factory> bindViewModelProviderFactoryFunction() {
        return HasDefaultViewModelProviderFactory::getDefaultViewModelProviderFactory;
    }

}
