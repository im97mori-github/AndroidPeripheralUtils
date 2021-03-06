package org.im97mori.ble.android.peripheral.ui.main;

import androidx.annotation.NonNull;

import org.im97mori.ble.android.peripheral.hilt.repository.DeviceSettingRepository;
import org.im97mori.ble.android.peripheral.room.DeviceSetting;

import java.util.List;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import io.reactivex.rxjava3.core.Completable;
import io.reactivex.rxjava3.functions.Action;
import io.reactivex.rxjava3.functions.Consumer;
import io.reactivex.rxjava3.subjects.PublishSubject;

@HiltViewModel
public class FakeMainViewModel extends MainViewModel {

    public final PublishSubject<List<DeviceSetting>> mObserveAllDeviceSettingSubject = PublishSubject.create();

    public Action mObserveDeleteAllDeviceSettingAction;

    @Inject
    FakeMainViewModel(@NonNull DeviceSettingRepository deviceSettingRepository) {
        super(deviceSettingRepository);
    }

    @Override
    public void observeLoadAllDeviceSetting(Consumer<List<DeviceSetting>> onNext, @NonNull Consumer<Throwable> onError) {
        mDisposable.add(mObserveAllDeviceSettingSubject.subscribe(onNext, onError));
    }

    @Override
    public void observeDeleteAllDeviceSetting(@NonNull Action onComplete, @NonNull Consumer<Throwable> onError) {
        if (mObserveDeleteAllDeviceSettingAction == null) {
            super.observeDeleteAllDeviceSetting(onComplete, onError);
        } else {
            mDisposable.add(Completable.fromAction(mObserveDeleteAllDeviceSettingAction).subscribe(onComplete, onError));
        }
    }

}