package org.im97mori.ble.android.peripheral.ui.device.setting;

import android.content.Intent;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.SavedStateHandle;

import org.im97mori.ble.android.peripheral.hilt.repository.FakeDeviceSettingRepository;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import io.reactivex.rxjava3.core.Single;
import io.reactivex.rxjava3.functions.Action;
import io.reactivex.rxjava3.functions.Consumer;
import io.reactivex.rxjava3.subjects.PublishSubject;

@HiltViewModel
public class FakeDeviceSettingViewModel extends DeviceSettingViewModel {

    public final PublishSubject<String> mObserveSetupSubject = PublishSubject.create();
    public final PublishSubject<String> mFragmentReadySubject = PublishSubject.create();
    public java.util.function.Consumer<String> mUpdateDeviceSettingNameConsumer;
    public java.util.function.Consumer<byte[]> mUpdateMockDataStringConsumer;

    public final FakeDeviceSettingRepository mFakeDeviceSettingRepository;

    @Inject
    FakeDeviceSettingViewModel(@NonNull SavedStateHandle savedStateHandle
            , @NonNull FakeDeviceSettingRepository deviceSettingRepository) {
        super(savedStateHandle, deviceSettingRepository);
        mFakeDeviceSettingRepository = deviceSettingRepository;
    }

    @Override
    public void observeSetup(@NonNull Intent intent, @NonNull Action onComplete, @NonNull Consumer<? super Throwable> onError) {
        mDisposable.add(mObserveSetupSubject
                .subscribe(s
                        -> mDisposable.add(Single.create(emitter -> emitter.onSuccess(s)).subscribe(o -> super.observeSetup(intent, onComplete, onError))))
        );
    }

    @Override
    public void updateDeviceSettingName(@Nullable String text) {
        if (mUpdateDeviceSettingNameConsumer != null) {
            mUpdateDeviceSettingNameConsumer.accept(text);
        }
        super.updateDeviceSettingName(text);
    }

    @Override
    public void updateMockData(@Nullable byte[] data) {
        if (mUpdateMockDataStringConsumer != null) {
            mUpdateMockDataStringConsumer.accept(data);
        }
        super.updateMockData(data);
    }

    @Override
    public void fragmentReady() {
        mDisposable.add(mFragmentReadySubject.subscribe(o
                -> mDisposable.add(Single.create(emitter -> emitter.onSuccess(o)).subscribe(o1 -> super.fragmentReady()))));
    }

}