package org.im97mori.ble.android.peripheral.ui.device.setting;

import android.content.Intent;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.SavedStateHandle;

import com.google.gson.Gson;

import org.im97mori.ble.android.peripheral.hilt.repository.FakeDeviceSettingRepository;

import java.util.function.Supplier;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import io.reactivex.rxjava3.core.Completable;
import io.reactivex.rxjava3.functions.Action;
import io.reactivex.rxjava3.functions.Consumer;
import io.reactivex.rxjava3.processors.PublishProcessor;

@HiltViewModel
public class FakeDeviceSettingViewModel extends DeviceSettingViewModel {

    public final PublishProcessor<String> mObserveSetupProcessor = PublishProcessor.create();
    public final PublishProcessor<Object> mFragmentReadyProcessor = PublishProcessor.create();
    public java.util.function.Consumer<String> mUpdateDeviceSettingNameConsumer;
    public java.util.function.Consumer<String> mObserveSaveConsumer;

    public final FakeDeviceSettingRepository mFakeDeviceSettingRepository;

    @Inject
    FakeDeviceSettingViewModel(@NonNull SavedStateHandle savedStateHandle
            , @NonNull FakeDeviceSettingRepository deviceSettingRepository
            , @NonNull Gson gson) {
        super(savedStateHandle, deviceSettingRepository, gson);
        mFakeDeviceSettingRepository = deviceSettingRepository;
    }

    @Override
    public void observeSetup(@NonNull Intent intent, @NonNull Action onComplete, @NonNull Consumer<? super Throwable> onError) {
        mDisposable.add(Completable.fromPublisher(mObserveSetupProcessor).subscribe(()
                -> super.observeSetup(intent, onComplete, onError)));
    }

    @Override
    public void updateDeviceSettingName(@Nullable String text) {
        if (mUpdateDeviceSettingNameConsumer != null) {
            mUpdateDeviceSettingNameConsumer.accept(text);
        }
        super.updateDeviceSettingName(text);
    }

    @Override
    public void fragmentReady() {
        mDisposable.add(Completable.fromPublisher(mFragmentReadyProcessor).subscribe(super::fragmentReady));
    }

    @Override
    public void observeSave(@NonNull Supplier<String> supplier, @NonNull Action onComplete, @NonNull Consumer<? super Throwable> onError) {
        if (mObserveSaveConsumer != null) {
            mObserveSaveConsumer.accept(supplier.get());
        } else {
            super.observeSave(supplier, onComplete, onError);
        }
    }

}