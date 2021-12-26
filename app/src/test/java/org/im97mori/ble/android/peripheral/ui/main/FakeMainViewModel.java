package org.im97mori.ble.android.peripheral.ui.main;

import androidx.annotation.NonNull;

import org.im97mori.ble.android.peripheral.hilt.repository.DeviceRepository;
import org.im97mori.ble.android.peripheral.room.DeviceSetting;

import java.util.List;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import io.reactivex.rxjava3.core.Completable;
import io.reactivex.rxjava3.functions.Action;
import io.reactivex.rxjava3.functions.Consumer;
import io.reactivex.rxjava3.processors.PublishProcessor;

@HiltViewModel
public class FakeMainViewModel extends MainViewModel {

    public final PublishProcessor<List<DeviceSetting>> mObserveAllDeviceSettingsProcessor = PublishProcessor.create();

    public Action mObserveDeleteAllDeviceSettingsAction;

    @Inject
    FakeMainViewModel(@NonNull DeviceRepository deviceRepository) {
        super(deviceRepository);
    }

    @Override
    public void observeAllDeviceSettings(io.reactivex.rxjava3.functions.Consumer<List<DeviceSetting>> onNext, @NonNull Consumer<Throwable> onError) {
        mDisposable.add(mObserveAllDeviceSettingsProcessor.subscribe(onNext, onError));
    }

    @Override
    public void observeDeleteAllDeviceSettings(@NonNull Action onComplete, @NonNull Consumer<Throwable> onError) {
        if (mObserveDeleteAllDeviceSettingsAction == null) {
            super.observeDeleteAllDeviceSettings(onComplete, onError);
        } else {
            mDisposable.add(Completable.fromAction(mObserveDeleteAllDeviceSettingsAction).subscribe(onComplete, onError));
        }
    }

}