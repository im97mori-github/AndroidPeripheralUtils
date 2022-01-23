package org.im97mori.ble.android.peripheral.ui.device.setting.fragment.blp;

import android.content.Intent;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.SavedStateHandle;

import com.google.gson.Gson;

import org.im97mori.ble.android.peripheral.hilt.repository.DeviceSettingRepository;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import io.reactivex.rxjava3.functions.Consumer;
import io.reactivex.rxjava3.subjects.PublishSubject;

@HiltViewModel
public class FakeBloodPressureProfileViewModel extends BloodPressureProfileViewModel {

    private final SavedStateHandle mSavedStateHandle;

    public final PublishSubject<String> mObserveSaveSubject = PublishSubject.create();

    @Inject
    public FakeBloodPressureProfileViewModel(@NonNull SavedStateHandle savedStateHandle
            , @NonNull DeviceSettingRepository deviceSettingRepository
            , @NonNull Gson gson) {
        super(savedStateHandle, deviceSettingRepository, gson);
        mSavedStateHandle = savedStateHandle;
    }

    @Override
    public void save(@NonNull Consumer<? super Throwable> onError) {
        mDisposable.add(mObserveSaveSubject.subscribe(s -> mSavedStateHandle.set("KEY_SAVED_DATA", s), onError));
    }

}
