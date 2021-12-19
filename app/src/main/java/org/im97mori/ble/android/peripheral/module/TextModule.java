package org.im97mori.ble.android.peripheral.module;

import androidx.annotation.NonNull;

import org.im97mori.ble.android.peripheral.datasource.ResourceTextSource;

import javax.inject.Singleton;

import dagger.Module;
import dagger.Provides;

@Module
public class TextModule {

    private final ResourceTextSource mResourceTextSource;

    public TextModule(@NonNull ResourceTextSource resourceTextSource) {
        mResourceTextSource = resourceTextSource;
    }

    @Provides
    @Singleton
    public ResourceTextSource provideResourceTextSource() {
        return mResourceTextSource;
    }

}
